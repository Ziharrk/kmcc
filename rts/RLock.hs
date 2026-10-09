module RLock (RLock, new, with) where

import           Control.Concurrent.RLock (RLock)
import qualified Control.Concurrent.RLock as RL
import System.IO.Unsafe (unsafePerformIO)

new :: IO RLock
new = RL.new

with :: Bool -> RLock -> IO a -> a
with True  rlock action = unsafePerformIO $ RL.with rlock action
with False _     action = unsafePerformIO action
