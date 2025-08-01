module Moongle.Server (server) where

import Effectful
import Effectful.Error.Static
import Effectful.Log
import Effectful.Reader.Static
import Effectful.Servant
import Moongle.Env
import Moongle.Server.Handler
import Moongle.Server.Types
import Network.Wai.Handler.Warp as Warp
import Servant qualified as S

serverT :: (Reader Env :> es, Error S.ServerError :> es, Log :> es) => S.ServerT MoongleAPI (Eff es)
serverT = searchHandler S.:<|> statsHandler

server :: (Reader Env :> es, Log :> es, IOE :> es) => Eff es ()
server = runWarpServerSettings @MoongleAPI Warp.defaultSettings serverT
