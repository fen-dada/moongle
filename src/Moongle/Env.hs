module Moongle.Env where

import Data.Time (UTCTime)

-- | The running environment for Moongle.
newtype Env = Env {lastUpdate :: UTCTime}
  deriving (Show, Eq)

