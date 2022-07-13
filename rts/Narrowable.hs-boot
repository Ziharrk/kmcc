module Narrowable where

import Data.Kind

class Narrowable (a :: Type)

instance Narrowable Bool
