{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Log
import Effectful.Reader.Static
import Effectful.Wreq
import Moongle.Server
import Log.Backend.StandardOutput
import Moongle.Config
import Moongle.Registry
import Moongle.Query
import Moongle.Env

-- main :: IO ()
-- main = do
--   raw <- BL.readFile "0.2.0.zip"
--   let result = unZip raw
--   case result of
--     Left err -> putStrLn $ "Error: " ++ show err
--     Right fs -> do
--       print $ fst <$> fs

testConfig :: Config
testConfig =
  Config
    { _registryUrl = "https://mooncakes.io/assets/modules.json",
      _moongleStoragePath = "/Users/hank/.moongle",
      _mooncakesBaseUrl = "https://moonbitlang-mooncakes.s3.us-west-2.amazonaws.com/user",
      _parallel = 32
    }

serverTest :: (Error String :> es, Log :> es, Reader Config :> es, FileSystem :> es, Concurrent :> es, Wreq :> es, IOE :> es) => Eff es ()
serverTest = do
  fetchAll
  mbtis <- parseAllMbti
  runReader (Env mbtis) server

runTest :: Eff '[Concurrent, FileSystem, Wreq, Reader Config, Log, Error String, IOE] () -> IO ()
runTest action = do
  a <- runEff $ withStdOutLogger $ \stdoutLogger ->
    runErrorNoCallStack $ runLog "demo" stdoutLogger defaultLogLevel $ runReader testConfig $ runWreq $ runFileSystem $ runConcurrent action
  case a of
    Left err -> putStrLn $ "Error: " ++ err
    Right _ -> putStrLn "Test completed successfully"


main :: IO ()
main = runTest serverTest
