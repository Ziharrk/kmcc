module Curry.FrontendUtils where

import Base.Messages ( warnOrAbort, abortWithMessages )
import Curry.Base.Monad ( runCYIO, CYIO )
import qualified CompilerOpts as Frontend
import CompilerOpts ( Options(..) )

runCurryFrontendAction :: Frontend.Options -> CYIO a -> IO a
runCurryFrontendAction opts act =
  runCYIO act >>= either abortWithMessages continueWithMessages
  where
    continueWithMessages (results, msgs) =
      warnOrAbort (optWarnOpts opts) msgs >> return results
