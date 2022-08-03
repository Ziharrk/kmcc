{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module Curry.Analysis where

import Control.Arrow ( first, second )
import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Except ( ExceptT (..), MonadError (..), runExceptT )
import Control.Monad.State ( StateT (..), MonadState, evalStateT, get, put, modify' )
import Data.Binary ( decodeFileOrFail, Binary, encodeFile )
import Data.Map ( Map )
import qualified Data.Map as Map ( empty, restrictKeys, lookup, insert, findWithDefault, unions, union, map, fromList )
import Data.Set ( Set )
import qualified Data.Set as Set ( empty, insert )
import GHC.Generics ( Generic )
import System.FilePath ( replaceExtension )

import Curry.Base.Message ( Message(..) )
import Base.Messages ( abortWithMessages, status )
import Curry.FlatCurry.Typed.Type ( TProg, QName, TRule (..), Visibility (..) )
import Curry.FlatCurry.Typed.Goodies ( trTProg, trTFunc, trTExpr )
import Curry.Base.Ident ( ModuleIdent )
import CurryBuilder ( smake, compMessage )
import CompilerOpts ( Options(..) )
import Curry.Files.Filenames ( addOutDirModule )
import Options ( KMCCOpts (frontendOpts, optOptimizationDeterminism), dumpMessage )

type NDAnalysisResult = Map QName NDInfo

data Locality = Local | Global

type family NDAnalysisState s = a | a -> s where
  NDAnalysisState 'Local  = (NDAnalysisResult, Set QName)
  NDAnalysisState 'Global = NDAnalysisResult

data NDInfo = Det | FlatNonDet | NonDet
  deriving (Binary, Generic, Eq, Ord, Show, Bounded)

newtype AM s a = AnalysisMonad {
    runAM :: StateT (NDAnalysisState s) (ExceptT [Message] IO) a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO
                     , MonadError [Message] )

deriving newtype instance (x ~ NDAnalysisState s) => MonadState x (AM s)

analyzeNondet :: [(TProg, ModuleIdent, FilePath)] -> KMCCOpts
              -> IO NDAnalysisResult
analyzeNondet tps kmccopts = handleRes $ runExceptT $ flip evalStateT Map.empty $ runAM $ Map.unions <$>
    mapM process' (zip tps [1..])
  where
    total = length tps
    process' ((tprog, m, fn), idx) = process kmccopts (idx, total) tprog m fn deps
      where deps = map (\(_, _, dep) -> dep) (take idx tps)
    handleRes act = act >>= \case
      Left msgs -> abortWithMessages msgs
      Right res -> return res

-- |Analyze a single flat curry module.
process :: KMCCOpts -> (Int, Int) -> TProg
        -> ModuleIdent -> FilePath -> [FilePath] -> AM 'Global NDAnalysisResult
process kmccopts idx tprog m fn deps
  -- if we are not doing determinism analysis,
  -- go to compile so that we can fill the analysis infos with default values.
  | not (optOptimizationDeterminism kmccopts) ||
    optForce opts = compile
  | otherwise     = smake [destFile] deps compile skip
  where
    opts = frontendOpts kmccopts
    destFile = tgtDir (analysisName fn)
    skip = do
      status opts $ compMessage idx (11, 16) "Skipping" m (fn, destFile)
      eithRes <- liftIO $ decodeFileOrFail (tgtDir (analysisName fn))
      case eithRes of
        Left (_, err) -> do
          liftIO $ putStr $ unlines
            [ "Binary analysis file is corrupted."
            , "For the file \"" ++ fn ++ "\"."
            , "Decoding failed with:"
            , err
            , "Retrying analysis from flat curry..." ]
          compile
        Right (analysis, exportedNames) -> do
          modify' (Map.union (Map.restrictKeys analysis exportedNames))
          liftIO $ dumpMessage kmccopts $ "Loaded cached analysis:\n" ++ show analysis
          return analysis
    compile = do
      res@(analysis, _) <- runLocalState $ do
        status opts $ compMessage idx (11, 16) "Analyzing" m (fn, destFile)
        analyzeTProg kmccopts tprog
      liftIO $ encodeFile (tgtDir (analysisName fn)) res
      return analysis

    tgtDir = addOutDirModule (optUseOutDir opts) (optOutDir opts) m

runLocalState :: AM 'Local a -> AM 'Global (NDAnalysisResult, Set QName)
runLocalState a = do
  st <- get
  let handle act = liftIO act >>= \case
        Left  msgs      -> throwError msgs
        Right (_, (allInfos, exportedQNames)) -> do
          put (Map.union st (Map.restrictKeys allInfos exportedQNames))
          return (allInfos, exportedQNames)
  handle $ runExceptT $ flip runStateT (st, Set.empty) $ runAM a

analyzeTProg :: KMCCOpts -> TProg -> AM 'Local ()
analyzeTProg opts = trTProg $ \_ _ _ funcs _ -> do
  mapM_ (trTFunc initAnalysis) funcs -- do an initial run to initialize all functions and collect visibility infos
  if optOptimizationDeterminism opts
    then fixedPoint (or <$> mapM (trTFunc analyzeFunc) funcs)
    else modify' (first (Map.map (const NonDet))) -- fill infos with default value

initAnalysis :: QName -> a -> Visibility -> c -> TRule -> AM 'Local ()
initAnalysis qname _ vis _ rule = do
  (mp, _) <- get
  when (vis == Public) $ modify' (second (Set.insert qname))
  modify' (first (Map.insert qname (checkDeterministic rule mp)))

fixedPoint :: Monad m => m Bool -> m ()
fixedPoint act = do
  b <- act
  when b $ fixedPoint act

analyzeFunc :: QName -> a -> Visibility -> c -> TRule -> AM 'Local Bool
analyzeFunc qname _ _ _ rule = do
  (mp, _) <- get
  case Map.lookup qname mp of
    Just NonDet -> return False
    cur -> do
      let res = checkDeterministic rule mp
      if cur == Just res
        then return False
        else modify' (first (Map.insert qname res)) >> return True

checkDeterministic :: TRule -> NDAnalysisResult -> NDInfo
checkDeterministic (TRule _ expr) mp = trTExpr var lit comb lt free o cse branch typed expr
  where
    var _ _ = Det
    lit _ _ = Det
    comb _ _ name args = max (Map.findWithDefault Det name mp) (maximum (minBound : args))
    lt bs e = max e (foldr (max . snd) Det bs)
    free _ _ = NonDet
    o _ _ = NonDet
    cse _ e bs = max e (maximum (minBound : bs))
    branch _ e = e
    typed e _ = e
checkDeterministic (TExternal _ ext) _ = Map.findWithDefault Det ext externalInfoMap

externalInfoMap :: Map String NDInfo
externalInfoMap = Map.fromList
  [ ("Prelude.=:<=", FlatNonDet)
  , ("Prelude.=:=" , FlatNonDet)
  , ("Prelude.$##" , FlatNonDet)
  ]

-- |Compute the filename of the analysis file for a source file
analysisName :: FilePath -> FilePath
analysisName = flip replaceExtension analysisExt

-- |Filename extension for analysis files
analysisExt :: String
analysisExt = ".an"
