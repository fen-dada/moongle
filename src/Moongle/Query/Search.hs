{-# LANGUAGE OverloadedStrings #-}

module Moongle.Query.Search
  ( buildTsQuery
  , buildTsQueryStrict
  , buildTsQueryStrictWith
  , queryDefSummary
  , query
  , queryType
  , queryText
  , defSummaryToSearchHit
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static (Error, throwError)
import Effectful.Log (Log, logInfo_)
import Effectful.PostgreSQL (WithConnection)
import Moongle.DB.Definition (DefSummary (..))
import Moongle.DB.Query (selectByTsQuery)
import Moongle.Query.Parser (text2NmQuery, text2Query, text2TyQuery)
import Moongle.Query.Syntax (Query, queryModifiers)
import Moongle.Server.Types (SearchHit (..))
import Moongle.TypeSearch (lexemesForQuery)

escapeTsqueryLexeme :: Text -> Text
escapeTsqueryLexeme = T.replace "'" "''"

buildTsQueryStrict :: [Text] -> Text
buildTsQueryStrict = buildTsQueryStrictWith (const False)

buildTsQuery :: [Text] -> Text
buildTsQuery = buildTsQueryStrictWith (T.isSuffixOf ".")

buildTsQueryStrictWith :: (Text -> Bool) -> [Text] -> Text
buildTsQueryStrictWith shouldPrefix toks =
  T.intercalate " & " (map one toks)
 where
  one t =
    let lit = "'" <> escapeTsqueryLexeme t <> "'"
     in if shouldPrefix t then lit <> ":*" else lit

queryDefSummary :: (WithConnection :> es, Log :> es, IOE :> es) => Query -> Eff es [DefSummary]
queryDefSummary q = do
  let qlxm = lexemesForQuery q
  let tsq = buildTsQuery qlxm
  logInfo_ $ "Querying with tsquery: " <> tsq
  selectByTsQuery tsq (queryModifiers q)

query :: (WithConnection :> es, Log :> es, IOE :> es, Error String :> es) => Text -> Eff es [SearchHit]
query t = do
  q <- either (throwError . show) pure (text2Query t)
  defSummaryToSearchHit <$>> queryDefSummary q

queryType :: (WithConnection :> es, Log :> es, IOE :> es, Error String :> es) => Text -> Eff es [SearchHit]
queryType t = do
  q <- either (throwError . show) pure (text2TyQuery t)
  defSummaryToSearchHit <$>> queryDefSummary q

queryText :: (WithConnection :> es, Log :> es, IOE :> es, Error String :> es) => Text -> Eff es [SearchHit]
queryText t = do
  q <- either (throwError . show) pure (text2NmQuery t)
  defSummaryToSearchHit <$>> queryDefSummary q

(<$>>) :: Functor f => (a -> b) -> f [a] -> f [b]
(<$>>) f = fmap (map f)

defSummaryToSearchHit :: DefSummary -> SearchHit
defSummaryToSearchHit DefSummary{username, mod = moduleName, package, prettySig} =
  SearchHit
    { user = username
    , mod = moduleName
    , package = package
    , decl = prettySig
    , score = 0
    }
