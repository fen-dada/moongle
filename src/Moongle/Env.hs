module Moongle.Env where

import Language.Moonbit.Mbti.Syntax
import Data.Time (UTCTime)
import Data.Text (Text)

-- | The running environment for Moongle.
data Env = Env {index :: [MbtiFile], lastUpdate :: UTCTime}
  deriving (Show, Eq)

