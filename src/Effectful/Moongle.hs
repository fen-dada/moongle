{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.Moongle where

import Data.Text (Text)
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Log
import Effectful.PostgreSQL (WithConnection)
import Moongle.Query (query, queryText, queryType)
import Moongle.Server.Types (SearchHit)

-- | An effect for interacting with Moongle
data Moongle :: Effect where
  TextSearch :: Text -> Moongle m [SearchHit]
  TypeSearch :: Text -> Moongle m [SearchHit]
  Search :: Text -> Moongle m [SearchHit]

type instance DispatchOf Moongle = Dynamic

textSearch :: (Moongle :> es) => Text -> Eff es [SearchHit]
textSearch = send . TextSearch

typeSearch :: (Moongle :> es) => Text -> Eff es [SearchHit]
typeSearch = send . TypeSearch

search :: (Moongle :> es) => Text -> Eff es [SearchHit]
search = send . Search

runMoongle :: (WithConnection :> es, Log :> es, IOE :> es, Error String :> es) => Eff (Moongle : es) [SearchHit] -> Eff es [SearchHit]
runMoongle = interpret_ \case
  TextSearch q -> queryText q
  TypeSearch q -> queryType q
  Search q -> query q
