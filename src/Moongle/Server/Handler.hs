{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Moongle.Server.Handler where

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Effectful
import Effectful.Error.Static
import Effectful.Log
import Effectful.PostgreSQL (WithConnection)
import Effectful.Reader.Static
import Moongle.DB
import Moongle.Env
import Moongle.Query
import Moongle.Query.Parser
import Moongle.Server.Types
import Servant qualified as S
import Text.Parsec (parse)
import Prelude hiding (mod)

searchHandler :: (WithConnection :> es, Log :> es, Reader Env :> es, Error S.ServerError :> es, IOE :> es) => ApiRequest SearchReq -> Eff es (ApiResponse SearchRes)
searchHandler (ApiRequest (SearchReq qt _ _ _ _)) = do
  case parse pQuery "" (LT.fromStrict qt) of
    Left perr -> do
      logInfo_ $ "Query parse error: " <> T.pack (show perr)
      throwError S.err400
    Right q -> do
      defsums <- queryDefSummary q

      let hits :: [SearchHit]
          hits = fmap defSummaryToSearchHit defsums

      let res = SearchRes (length hits) hits
      logInfo_ $ "Search query: " <> qt <> ", found " <> T.pack (show (length hits)) <> " results"
      pure $ ApiResponse "ok" (Just res) Nothing
 where
  defSummaryToSearchHit :: DefSummary -> SearchHit
  defSummaryToSearchHit DefSummary{..} =
    SearchHit
      { user = username
      , mod = mod
      , package = package
      , decl = prettySig
      , score = 0
      }

statsHandler :: (Log :> es, Reader Env :> es, WithConnection :> es, IOE :> es) => Eff es (ApiResponse Stats)
statsHandler = do
  (decls', modules') <- getStats
  let decls = fromIntegral decls'
  let modules = fromIntegral modules'
  logInfo_ $ "Stats query: " <> T.pack (show decls) <> " defs, " <> T.pack (show modules) <> " packages"
  Env{lastIndexed} <- ask @Env
  pure $ ApiResponse "ok" (Just $ Stats{..}) Nothing
