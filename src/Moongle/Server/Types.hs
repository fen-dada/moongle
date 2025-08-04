{-# LANGUAGE DuplicateRecordFields #-}

module Moongle.Server.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Servant
import Language.Moonbit.Mbti.Syntax

data ApiError = ApiError
  { code :: T.Text,
    message :: T.Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ApiError

instance FromJSON ApiError

data ApiResponse a = ApiResponse
  { status :: T.Text,
    dat :: Maybe a,
    err :: Maybe ApiError
  }
  deriving (Show, Eq, Generic)

instance (ToJSON a) => ToJSON (ApiResponse a)

instance (FromJSON a) => FromJSON (ApiResponse a)

newtype ApiRequest a = ApiRequest {dat :: a}
  deriving (Show, Eq, Generic)

instance (FromJSON a) => FromJSON (ApiRequest a)

instance (ToJSON a) => ToJSON (ApiRequest a)

data SearchReq = SearchReq
  { q :: T.Text,
    limit :: Maybe Int,
    offset :: Maybe Int,
    lang :: Maybe T.Text,
    filters :: Maybe SearchFilters
  }
  deriving (Show, Eq, Generic)

instance FromJSON SearchReq

instance ToJSON SearchReq

data SearchFilters = SearchFilters
  { versions :: Maybe [T.Text],
    visibility :: Maybe T.Text,
    effects :: Maybe [T.Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SearchFilters

instance ToJSON SearchFilters

data SearchHit = SearchHit
  { user :: T.Text,
    mod :: T.Text,
    package :: [T.Text],
    decl :: T.Text,
    -- doc :: Maybe T.Text,
    score :: Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON SearchHit

instance ToJSON SearchHit

data SearchRes = SearchRes
  { hitsTotal :: Int,
    items :: [SearchHit]
  }
  deriving (Show, Eq, Generic)

instance FromJSON SearchRes

instance ToJSON SearchRes

data Stats = Stats
  { modules :: Int,
    decls :: Int,
    lastIndexed :: UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON Stats

instance ToJSON Stats

type JSONReq a = ReqBody '[JSON] (ApiRequest a)

type SearchAPI = "search" :> JSONReq SearchReq :> Post '[JSON] (ApiResponse SearchRes)

type StatsAPI = "stats" :> Get '[JSON] (ApiResponse Stats)

type MoongleAPI = "api" :> (SearchAPI :<|> StatsAPI)
