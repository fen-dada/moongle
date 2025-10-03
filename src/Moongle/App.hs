{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Moongle.App
  ( AppEffects
  , runApp
  , runUpdate
  , runServe
  , runSearch
  ) where

import Control.Lens ((^.))
import Data.Text qualified as T
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log, logInfo_)
import Effectful.PostgreSQL (WithConnection)
import Effectful.Reader.Static (Reader, asks)
import Moongle.CLI (Cmd (..), SearchOpts (..), ServeOpts, UpdateOpts)
import Moongle.Config (Config, parallel)
import Moongle.Env (Env)
import Moongle.Query.Indexer (parseAllMbtiWithPkg, upsertPackages)
import Moongle.Query.Search qualified as Query
import Moongle.Registry.Fetch (fetchAllPackages)
import Moongle.Server (server)

import Moongle.DB.Migrations (runMigrations)

-- | All effects required by the CLI commands. We over-approximate slightly so
-- the dispatcher can be polymorphic across commands.
type AppEffects es =
  ( Reader Env :> es
  , Reader Config :> es
  , Error String :> es
  , Log :> es
  , FileSystem :> es
  , Concurrent :> es
  , WithConnection :> es
  , IOE :> es
  )

runApp :: AppEffects es => Cmd -> Eff es ()
runApp = \case
  Update opts -> runUpdate opts
  Serve opts -> runServe opts
  Search opts -> runSearch opts

runUpdate :: AppEffects es => UpdateOpts -> Eff es ()
runUpdate _opts = do
  maxThreads <- asks (^. parallel)
  logInfo_ $ "Updating registry using up to " <> showText maxThreads <> " workers"
  runMigrations
  fetchAllPackages maxThreads
  mbtis <- parseAllMbtiWithPkg
  _ <- upsertPackages mbtis
  logInfo_ "Update finished"

runServe :: AppEffects es => ServeOpts -> Eff es ()
runServe = server

runSearch :: AppEffects es => SearchOpts -> Eff es ()
runSearch (SearchOpts str limit) = do
  result <- Query.query str
  liftIO $ print (take limit result)

showText :: Show a => a -> T.Text
showText = T.pack . show
