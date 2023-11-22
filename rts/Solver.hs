module Solver(startSolver, evalSymbolic, SolverState) where

import Control.Concurrent (MVar, newMVar, takeMVar, putMVar)
import Control.Exception (Exception, throwIO, catches, ErrorCall(..), Handler(..))
import Control.Monad.Except (MonadError, MonadTrans)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.RWS (MonadState(..), MonadWriter)
import Data.SBV.Internals (SolverContext(contextState), State)
import Data.SBV.Control (query, Query)
import Data.SBV (SMTConfig (verbose), runSMTWith)
import Unsafe.Coerce (unsafeCoerce)
import Data.SBV.Dynamic (defaultSMTCfg)

import Data.Typeable

type SolverState = (Either String State, MVar ())

-- | A hack to get the state of the solver context.
newtype StateHackException = SHE State

instance Show StateHackException where
  show :: StateHackException -> String
  show (SHE _) = "StateHackException <QueryState>"

instance Exception StateHackException

-- | A copy of the 'SymbolicT' from SBV, to have a version that doesn't close the solver context.
newtype MySymbolicT m a = MySymbolicT { runMySymbolicT :: ReaderT State m a }
  deriving ( Applicative, Functor, Monad, MonadIO, MonadTrans
           , MonadError e, MonadState s, MonadWriter w, MonadFail
           )

symbolicToMySymbolic :: Query a -> MySymbolicT IO a
symbolicToMySymbolic = unsafeCoerce

evalSymbolic :: SolverState -> Query a -> IO a
evalSymbolic (Left err, _) _ = putStrLn err >> putStrLn z3Msg >> error ""
evalSymbolic (Right st, sem) act = do
  takeMVar sem
  r <- runReaderT (runMySymbolicT (symbolicToMySymbolic act)) st
  putMVar sem ()
  return r

startSolver :: IO SolverState
startSolver = do
  sem <- newMVar ()
  catches (runSMTWith (defaultSMTCfg { verbose = False }) $ query $ do
    st <- contextState
    _ <- liftIO $ throwIO $ SHE st
    return (Right st, sem)) $ [
    Handler (\ (ex :: StateHackException) -> handleSHE     sem ex),
    Handler (\ (ex :: ErrorCall)          -> handleErrCall sem ex)
    ]

--- handler for StateHackExceptions
handleSHE :: MVar () -> StateHackException -> IO SolverState
handleSHE sem (SHE st) = return (Right st, sem)

--- handler for ErrorCall exceptions
handleErrCall :: MVar () -> ErrorCall -> IO SolverState
handleErrCall sem e = return (Left (show e), sem)

z3Msg :: String
z3Msg = unlines [
  "",
  "Please install z3 and add it to your PATH",
  "Consider installing it from https://github.com/Z3Prover/z3/releases"
  ]