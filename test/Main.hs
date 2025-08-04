{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Log
import Effectful.Reader.Static
import Effectful.Wreq
import Log.Backend.StandardOutput
import Moongle.Config
import Moongle.Env
import Moongle.Query
import Moongle.Registry
import Moongle.Server
import System.FilePath
import Data.Time

-- main :: IO ()
-- main = do
--   raw <- BL.readFile "0.2.0.zip"
--   let result = unZip raw
--   case result of
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right fs -> do
--       print $ fst <$> fs

makeTestConfig :: (FileSystem :> es) => Eff es Config
makeTestConfig = do
  homeDir <- getHomeDirectory
  let storagePath = homeDir </> ".moongle"
  pure
    Config
      { _registryUrl = "https://mooncakes.io/assets/modules.json",
        _moongleStoragePath = T.pack storagePath,
        _mooncakesBaseUrl = "https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com/user",
        _parallel = 32
      }

serverTest :: (Error String :> es, Log :> es, Reader Config :> es, FileSystem :> es, Concurrent :> es, Wreq :> es, IOE :> es) => Eff es ()
serverTest = do
  fetchAll
  mbtis <- parseAllMbti
  utcNow <- liftIO getCurrentTime
  runReader (Env mbtis utcNow) server

runTest :: Eff '[Concurrent, FileSystem, Wreq, Reader Config, Log, Error String, IOE] () -> IO ()
runTest action = do
  testConfig <- runEff $ runFileSystem makeTestConfig
  a <- runEff $ withStdOutLogger $ \stdoutLogger ->
    runErrorNoCallStack $ runLog "demo" stdoutLogger defaultLogLevel $ runReader testConfig $ runWreq $ runFileSystem $ runConcurrent action
  case a of
    Left err -> putStrLn $ "Error: " ++ err
    Right _ -> putStrLn "Test completed successfully"

main :: IO ()
main = runTest serverTest
