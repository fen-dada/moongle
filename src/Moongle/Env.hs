module Moongle.Env where

import Language.Moonbit.Mbti.Syntax

-- | The running environment for Moongle.
newtype Env = Env
  {index :: [MbtiFile]}
  deriving (Show, Eq)


