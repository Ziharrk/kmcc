{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module Curry.Analysis where

import Control.Arrow ( first )
import Control.Monad ( when )
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Except ( ExceptT (..), MonadError (..), runExceptT )
import Control.Monad.State ( StateT (..), MonadState, execStateT, modify, get, put )
import Data.Binary ( decodeFileOrFail, Binary )
import Data.Map ( Map )
import qualified Data.Map as Map ( empty, restrictKeys, lookup, insert, findWithDefault, fromList )
import Data.Set ( Set )
import qualified Data.Set as Set ( empty, insert )
import GHC.Generics ( Generic )
import System.FilePath ( replaceExtension )

import Curry.Base.Message ( Message(..) )
import Base.Messages ( abortWithMessages, status )
import Curry.FlatCurry.Typed.Type ( TProg, QName, TRule (..), Visibility (..) )
import qualified CompilerOpts as Frontend
import Curry.FlatCurry.Typed.Goodies ( trTProg, trTFunc, trTExpr )
import Curry.Base.Ident ( ModuleIdent )
import CurryBuilder ( smake, compMessage )
import CompilerOpts ( Options(..) )
import Curry.Files.Filenames ( typedBinaryFlatName, addOutDirModule )

type NDAnalysisResult = Map QName NDInfo

data Locality = Local | Global

type family NDAnalysisState s = a | a -> s where
  NDAnalysisState Local  = (NDAnalysisResult, Set QName)
  NDAnalysisState Global = NDAnalysisResult

data NDInfo = Det | NonDet
  deriving (Binary, Generic, Eq, Ord)

newtype AM s a = AnalysisMonad {
    runAM :: StateT (NDAnalysisState s) (ExceptT [Message] IO) a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO
                     , MonadError [Message] )

deriving newtype instance (x ~ NDAnalysisState s) => MonadState x (AM s)

analyzeNondet :: [(TProg, ModuleIdent, FilePath)] -> Frontend.Options
              -> IO NDAnalysisResult
analyzeNondet tps opts = handleRes $ runExceptT $ flip execStateT Map.empty $ runAM $
    mapM_ process' (zip tps [1..])
  where
    total = length tps
    deps = map (\(_, _, dep) -> dep) tps
    process' ((tprog, m, fn), idx) = process opts (idx, total) tprog m fn deps
    handleRes act = act >>= \case
      Left msgs -> abortWithMessages msgs
      Right res -> return res

-- |Analyze a single flat curry module.
process :: Frontend.Options -> (Int, Int) -> TProg
        -> ModuleIdent -> FilePath -> [FilePath] -> AM Global ()
process opts idx tprog m fn deps
  | optForce opts = compile
  | otherwise     = smake [destFile] deps compile skip
  where
    destFile = tgtDir (analysisName fn)
    skip = do
      status opts $ compMessage idx "Skipping" m (fn, destFile)
      eithRes <- liftIO $ decodeFileOrFail (tgtDir (analysisName fn))
      case eithRes of
        Left (_, err) -> do
          liftIO $ putStrLn $ unwords
            [ "Binary analysis file is corrupted."
            , "For the file\"" ++ fn ++ "\"."
            , "Decoding failed with:"
            , err
            , "Retrying analysis from flat curry..." ]
          compile
        Right res -> modify (res <>)
    compile = toGlobalState $ do
      status opts $ compMessage idx "Compiling" m (fn, destFile)
      analyzeTProg tprog
      -- TODO: 1. write analysis file
      -- TODO: 2. currently only returning the result of the current module.

    tgtDir = addOutDirModule (optUseOutDir opts) (optOutDir opts) m

toGlobalState :: AM Local a -> AM Global a
toGlobalState a = get >>= \st -> handle $ runExceptT $ flip runStateT (st, Set.empty) $ runAM a
  where
    handle act = liftIO act >>= \case
      Left  msgs      -> throwError msgs
      Right (res, (allInfos, exportedQNames)) -> put (Map.restrictKeys allInfos exportedQNames) >> return res

analyzeTProg :: TProg -> AM Local ()
analyzeTProg = trTProg (\_ _ _ funcs _ -> fixedPoint (or <$> mapM (trTFunc analyzeFunc) funcs))

fixedPoint :: Monad m => m Bool -> m ()
fixedPoint act = do
  b <- act
  when b $ fixedPoint act

analyzeFunc :: QName -> a -> Visibility -> c -> TRule -> AM Local Bool
analyzeFunc qname _ vis _ rule = do
  (mp, st) <- get
  when (vis == Public) $ put (mp, Set.insert qname st)
  case Map.lookup qname mp of
    Just NonDet -> return False
    cur -> do
      let res = checkDeterministic rule mp
      if cur == Just res
        then return False
        else modify (first (Map.insert qname res)) >> return True

checkDeterministic :: TRule -> NDAnalysisResult -> NDInfo
checkDeterministic (TRule _ expr) mp = trTExpr var lit comb lt free o cse branch typed expr
  where
    var _ _ = Det
    lit _ _ = Det
    comb _ _ name args = max (Map.findWithDefault Det name mp) (maximum args)
    lt bs e = max e (foldr (max . snd) Det bs)
    free _ _ = NonDet
    o _ _ = NonDet
    cse _ e bs = max e (maximum bs)
    branch _ e = e
    typed e _ = e
checkDeterministic (TExternal _ ext) _ = Map.findWithDefault Det ext externalInfoMap

externalInfoMap :: Map String NDInfo
externalInfoMap = mempty -- Map.fromList []

-- |Compute the filename of the analysis file for a source file
analysisName :: FilePath -> FilePath
analysisName = flip replaceExtension analysisExt

-- |Filename extension for analysis files
analysisExt :: String
analysisExt = ".an"
