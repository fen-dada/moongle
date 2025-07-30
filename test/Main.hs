module Main (main) where

import Data.ByteString.Lazy qualified as BL
import Moongle.General.Utils

main :: IO ()
main = do
  raw <- BL.readFile "0.2.0.zip"
  let result = unZip raw
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right fs -> do
      print $ fst <$> fs
