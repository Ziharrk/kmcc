{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module Curry.Analysis where

import Control.Arrow ( first, second )
import Control.Monad ( when, msum, unless )
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
import System.Directory ( doesFileExist )

import Curry.Base.Message ( Message(..) )
import Curry.Frontend.Base.Messages ( abortWithMessages, status )
import Curry.FlatCurry.Typed.Type ( TProg, QName, TRule (..), Visibility (..) )
import Curry.FlatCurry.Typed.Goodies ( trTProg, trTFunc, trTExpr )
import Curry.Base.Ident ( ModuleIdent )
import Curry.Frontend.CurryBuilder ( compMessage )
import Curry.Frontend.CompilerOpts ( Options(..) )
import Curry.Files.Filenames ( addOutDirModule )
import Options ( KMCCOpts (frontendOpts, optOptimizationDeterminism), dumpMessage, debugMessage )

type NDAnalysisResult = Map QName NDInfo

data Locality = Local | Global

type family NDAnalysisState s = a | a -> s where
  NDAnalysisState 'Local  = (NDAnalysisResult, Set QName)
  NDAnalysisState 'Global = NDAnalysisResult

data NDInfo = Det | NonDet
  deriving (Binary, Generic, Eq, Ord, Show, Bounded)

data NDReason = None | FreeV | Choice | Fun QName
  deriving Show

instance Monoid NDReason where
  mempty = None

instance Semigroup NDReason where
  None <> r = r
  r <> _ = r

newtype AM s a = AnalysisMonad {
    runAM :: StateT (NDAnalysisState s) (ExceptT [Message] IO) a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO
                     , MonadError [Message] )

deriving newtype instance (x ~ NDAnalysisState s) => MonadState x (AM s)

analyzeNondet :: [((TProg, Bool), ModuleIdent, FilePath)] -> KMCCOpts
              -> IO NDAnalysisResult
analyzeNondet tps kmccopts = handleRes $ runExceptT $ flip evalStateT Map.empty $ runAM $ Map.unions <$>
    mapM process' (zip tps [1..])
  where
    total = length tps
    process' (((tprog, comp), m, fn), idx) = process kmccopts (idx, total) tprog comp m fn
    handleRes act = act >>= \case
      Left msgs -> abortWithMessages msgs
      Right res -> return res

-- |Analyze a single flat curry module.
process :: KMCCOpts -> (Int, Int) -> TProg -> Bool
        -> ModuleIdent -> FilePath -> AM 'Global NDAnalysisResult
process kmccopts idx@(thisIdx,maxIdx) tprog comp m fn
  -- if we are not doing determinism analysis,
  -- go to compile so that we can fill the analysis infos with default values.
  | not (optOptimizationDeterminism kmccopts) ||
    optForce opts ||
    comp      = compile
  | otherwise =  liftIO (doesFileExist destFile)
      >>= \exists -> if exists then skip else compile
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
          if thisIdx == maxIdx
            then liftIO $ dumpMessage kmccopts $ "Loaded cached analysis:\n" ++ show analysis
            else liftIO $ dumpMessage kmccopts "Loaded cached analysis."
          return analysis
    compile = do
      res@(analysis, _) <- runLocalState $ do
        -- status opts $ compMessage idx (11, 16) "Analyzing" m (fn, destFile)
        analyzeTProg kmccopts tprog
      liftIO $ encodeFile (tgtDir (analysisName fn)) res
      if thisIdx == maxIdx
        then liftIO $ dumpMessage kmccopts $ "Analysis finished:\n" ++ show analysis
        else liftIO $ dumpMessage kmccopts "Analysis finished."
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
  mapM_ (trTFunc (initAnalysis opts)) funcs -- do an initial run to initialize all functions and collect visibility infos
  if optOptimizationDeterminism opts
    then fixedPoint (or <$> mapM (trTFunc (analyzeFunc opts)) funcs)
    else modify' (first (Map.map (const NonDet))) -- fill infos with default value

initAnalysis :: KMCCOpts -> QName -> a -> Visibility -> c -> TRule -> AM 'Local ()
initAnalysis opts qname _ vis _ rule = do
  (mp, _) <- get
  when (vis == Public) $ modify' (second (Set.insert qname))
  let (res, reason) = checkDeterministic rule mp
  unless ('#' `elem` snd qname || res == Det) $ liftIO $ debugMessage opts (show (qname, res, reason))
  modify' (first (Map.insert qname res))

fixedPoint :: Monad m => m Bool -> m ()
fixedPoint act = do
  b <- act
  when b $ fixedPoint act

analyzeFunc :: KMCCOpts -> QName -> a -> Visibility -> c -> TRule -> AM 'Local Bool
analyzeFunc opts qname _ _ _ rule = do
  (mp, _) <- get
  case Map.lookup qname mp of
    Just NonDet -> return False
    cur -> do
      let (res, reason) = checkDeterministic rule mp
      if cur == Just res
        then return False
        else do
          unless ('#' `elem` snd qname) $ liftIO $ debugMessage opts (show (qname, res, reason))
          modify' (first (Map.insert qname res)) >> return True

checkDeterministic :: TRule -> NDAnalysisResult -> (NDInfo, Maybe NDReason)
checkDeterministic (TRule _ expr) mp = trTExpr var lit comb lt free o cse branch typed expr
  where
    var _ _ = (Det, Nothing)
    lit _ _ = (Det, Nothing)
    comb _ _ name args' = (max here (maximum (minBound : args)), if here == NonDet then Just (Fun name) else msum reasons)
      where (args, reasons) = unzip args'
            here = Map.findWithDefault Det name mp
    lt bs' (e, reason) = (max e (foldr (max . snd) Det bs), msum (reason:reasons))
      where
        (bs, reasons) = unzip $ map (\(a, (i,r)) -> ((a, i), r)) bs'
    free _ _ = (NonDet, Just FreeV)
    o _ _ = (NonDet, Just Choice)
    cse _ (e, reason) bs' = (max e (maximum (minBound : bs)), msum (reason:reasons))
      where
        (bs, reasons) = unzip bs'
    branch _ e = e
    typed e _ = e
checkDeterministic (TExternal _ ext) _ = (Map.findWithDefault Det ext externalInfoMap, Nothing)

externalInfoMap :: Map String NDInfo
externalInfoMap = Map.fromList
  [ ("Prelude.=:<=", NonDet)
  , ("Prelude.=:=" , NonDet)
  ]

-- |Compute the filename of the analysis file for a source file
analysisName :: FilePath -> FilePath
analysisName = flip replaceExtension analysisExt

-- |Filename extension for analysis files
analysisExt :: String
analysisExt = ".an"
