module RLock (RLock, new, with) where

import System.IO.Unsafe (unsafePerformIO)

import qualified Control.Concurrent.RLock as RL

newtype RLock = RLock RL.RLock

new :: IO RLock
new = RLock <$> RL.new

with :: RLock -> IO a -> a
with (RLock rlock) action = unsafePerformIO $ RL.with rlock action
