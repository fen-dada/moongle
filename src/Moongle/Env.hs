module Moongle.Env where

import Language.Moonbit.Mbti.Syntax
import Data.Time (UTCTime)

-- | The running environment for Moongle.
data Env = Env {index :: [MbtiFile], lastUpdate :: UTCTime}
  deriving (Show, Eq)

data Index = Index
  { 

  }
