module Moongle.Server (server) where

import Data.Function ((&))
import Data.String (fromString)
import Data.Text qualified as T
import Effectful
import Effectful.Error.Static
import Effectful.Log
import Effectful.PostgreSQL
import Effectful.Reader.Static
import Effectful.Servant
import Moongle.CLI (ServeOpts (..))
import Moongle.Env
import Moongle.Server.Handler
import Moongle.Server.Types
import Network.Wai.Handler.Warp as Warp
import Servant qualified as S

serverT :: (Reader Env :> es, Error S.ServerError :> es, Log :> es, IOE :> es, WithConnection :> es) => S.ServerT MoongleAPI (Eff es)
serverT = searchHandler S.:<|> statsHandler

server :: (Reader Env :> es, Log :> es, IOE :> es, WithConnection :> es) => ServeOpts -> Eff es ()
server (ServeOpts mHost mPort) = do
  let toHP :: T.Text -> Warp.HostPreference
      toHP = fromString . T.unpack

      settings =
        Warp.defaultSettings
          & maybe id (Warp.setHost . toHP) mHost
          & maybe id Warp.setPort mPort
  runWarpServerSettings @MoongleAPI settings serverT id
