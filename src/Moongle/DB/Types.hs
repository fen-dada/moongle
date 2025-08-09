{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Moongle.DB.Types where

import Data.Text (Text)
import GHC.Generics (Generic)

data DefRow = DefRow
  { pkgOwner   :: Text
  , pkgName    :: Text
  , pkgVersion :: Text

  , modUser    :: Text
  , modName    :: Text
  , modPkgPath :: [Text]

  , funName    :: Text
  , prettySig  :: Text
  , visibility :: Text
  , kind       :: Text

  , tokensLex  :: [Text]
  , tokensTSV  :: Text
  , arity      :: Int
  , hasAsync   :: Bool
  , mayRaise   :: Bool

  , srcFile    :: Maybe Text
  , srcLine    :: Maybe Int
  , srcCol     :: Maybe Int

  , versionTag :: Text
  } deriving (Show, Eq, Generic)

