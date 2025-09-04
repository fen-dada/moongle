module Moongle.Env where

import Data.Time (UTCTime)
import Network.HTTP.Client

-- | The running environment for Moongle.
data Env = Env {lastIndexed :: UTCTime, httpManager :: Manager}

