{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
module Curry.Analysis where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Except ( ExceptT (..), MonadError (..), runExceptT )
import Control.Monad.State ( StateT (..), MonadState, execStateT, modify, get, put )
import Data.Binary (decodeFileOrFail, Binary)
import Data.Map ( Map )
import qualified Data.Map as Map (empty, restrictKeys)
import Data.Set ( Set )
import qualified Data.Set as Set (empty)
import GHC.Generics (Generic)
import System.FilePath (replaceExtension)

import Curry.Base.Message (Message(..))
import Base.Messages (abortWithMessages, status)
import Curry.FlatCurry.Typed.Type ( TProg, QName, TRule )
import qualified CompilerOpts as Frontend
import Curry.FlatCurry.Typed.Goodies ( trTProg, trTFunc )
import Curry.Base.Ident (ModuleIdent)
import CurryBuilder (smake, compMessage)
import CompilerOpts (Options(..))
import Curry.Files.Filenames (typedBinaryFlatName, addOutDirModule)

type NDAnalysisResult = Map QName NDInfo

data Locality = Local | Global

type family NDAnalysisState s = a | a -> s where
  NDAnalysisState Local  = (NDAnalysisResult, Set QName)
  NDAnalysisState Global = NDAnalysisResult

data NDInfo = Det | Nondet
  deriving (Binary, Generic)

newtype AM s a = AnalysisMonad {
    runAM :: StateT (NDAnalysisState s) (ExceptT [Message] IO) a
  } deriving newtype ( Functor, Applicative, Monad, MonadIO
                     , MonadError [Message] )

deriving newtype instance (x ~ NDAnalysisState s) => MonadState x (AM s)

analyzeNondet :: [(TProg, ModuleIdent, FilePath)] -> [FilePath] -> Frontend.Options
              -> IO NDAnalysisResult
analyzeNondet tps deps opts = handleRes $ runExceptT $ flip execStateT (Map.empty) $ runAM $
    mapM_ process' (zip tps [1..])
  where
    process' ((tprog, m, fn), idx) = process opts (idx, length tps) m fn tprog deps
    handleRes :: IO (Either [Message] NDAnalysisResult) -> IO NDAnalysisResult
    handleRes act = act >>= \case
      Left msgs -> abortWithMessages msgs
      Right res -> return res

-- |Analyze a single flat curry module.
process :: Frontend.Options -> (Int, Int)
        -> ModuleIdent -> FilePath -> TProg -> [FilePath] -> AM Global ()
process opts idx m fn tprog deps
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

    tgtDir = addOutDirModule (optUseOutDir opts) (optOutDir opts) m

toGlobalState :: AM Local a -> AM Global a
toGlobalState a = get >>= \st -> handle $ runExceptT $ flip runStateT (st, Set.empty) $ runAM a
  where
    handle :: IO (Either [Message] (a, (Map QName NDInfo, Set QName))) -> AM Global a
    handle act = liftIO act >>= \case
      Left  msgs      -> throwError msgs
      Right (res, (allInfos, exportedQNames)) -> put (Map.restrictKeys allInfos exportedQNames) >> return res

analyzeTProg :: TProg -> AM Local ()
analyzeTProg = trTProg (\_ _ _ funcs _ -> mapM_ (trTFunc analyzeFunc) funcs)

analyzeFunc :: QName -> a -> b -> c -> TRule -> AM Local ()
analyzeFunc qname = undefined

-- |Compute the filename of the analysis file for a source file
analysisName :: FilePath -> FilePath
analysisName = flip replaceExtension analysisExt

-- |Filename extension for analysis files
analysisExt :: String
analysisExt = ".an"
