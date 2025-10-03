{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Moongle.DB.Definition
  ( DefSummary (..)
  ) where

import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray (..))

-- | A compact representation of a function definition stored in the database.
data DefSummary = DefSummary
  { username :: Text
  , mod :: Text
  , version :: Text
  , package :: [Text]
  , funName :: Text
  , prettySig :: Text
  }
  deriving (Show, Eq)

instance FromRow DefSummary where
  fromRow = do
    u <- field
    m <- field
    v <- field
    PGArray pkg <- field
    fn <- field
    ps <- field
    pure
      DefSummary
        { username = u
        , mod = m
        , version = v
        , package = pkg
        , funName = fn
        , prettySig = ps
        }
