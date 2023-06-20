{-# LANGUAGE MagicHash #-}
import qualified Prelude as P
import qualified System.CPUTime as P
import BasicDefinitions

cPUTimedotgetCPUTime_Det# = fromForeign (P.getCPUTime P.>>= (P.return P.. (`P.div` (10 P.^ 9))))
cPUTimedotgetCPUTime_ND# = P.error "No implementation of getCPUTime_ND"

cPUTimedotgetElapsedTime_Det# = P.error "No implementation of getElapsedTime_Det"
cPUTimedotgetElapsedTime_ND# = P.error "No implementation of getElapsedTime_ND"