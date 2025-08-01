{-# LANGUAGE OverloadedStrings #-}

module Moongle.Server.Handler where

import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Effectful
import Effectful.Error.Static
import Effectful.Log
import Effectful.Reader.Static
import Language.Moonbit.Mbti.Syntax
import Moongle.Env
import Moongle.Query
import Moongle.Query.Parser
import Moongle.Server.Types
import Servant qualified as S
import Text.FuzzyFind
import Text.Parsec (parse)

searchHandler :: (Log :> es, Reader Env :> es, Error S.ServerError :> es) => ApiRequest SearchReq -> Eff es (ApiResponse SearchRes)
searchHandler (ApiRequest (SearchReq qt _ _ _ _)) = do
  case parse pQuery "" (LT.fromStrict qt) of
    Left perr -> do
      logInfo_ $ "Query parse error: " <> T.pack (show perr)
      throwError S.err400
    Right q -> do
      QueryResult qrDecls <- query q
      let hits :: [SearchHit]
          hits = fmap toHits qrDecls

          toHits :: (Alignment, QueryEntry) -> SearchHit
          toHits (Alignment {score}, QueryEntry (ModulePath u m pkgs) decl) =
            SearchHit
              { user = nameText u,
                mod = nameText m,
                package = fmap nameText pkgs,
                decl = T.pack $ show decl,
                score = score
              }

          nameText :: Name -> T.Text
          nameText = T.pack . show

      let res = SearchRes (length hits) hits
      logInfo_ $ "Search query: " <> qt <> ", found " <> T.pack (show (length hits)) <> " results"
      pure $ ApiResponse "ok" (Just res) Nothing

statsHandler :: (Log :> es, Reader Env :> es, Error S.ServerError :> es) => Eff es (ApiResponse Stats)
statsHandler = do
  undefined
