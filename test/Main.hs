{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Effectful
import Effectful.Concurrent
import Effectful.Error.Static
import Effectful.FileSystem
import Effectful.Log
import Effectful.Reader.Static
import Effectful.Wreq
import Log.Backend.StandardOutput
import Moongle.Config
import Moongle.Registry
import Control.Lens

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

runTest :: Eff '[Concurrent, FileSystem, Wreq, Reader Config, Log, Error String, IOE] () -> IO ()
runTest action = do
  a <- runEff $ withStdOutLogger $ \stdoutLogger ->
    runErrorNoCallStack $ runLog "demo" stdoutLogger defaultLogLevel $ runReader testConfig $ runWreq $ runFileSystem $ runConcurrent action
  case a of
    Left err -> putStrLn $ "Error: " ++ err
    Right _ -> putStrLn "Test completed successfully"


main :: IO ()
main = runTest fetchAll
