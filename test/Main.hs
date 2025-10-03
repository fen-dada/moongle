{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Either (isLeft)
import Data.Text qualified as T
import Language.Moonbit.Mbti.Syntax
import Moongle.Query (buildTsQuery, buildTsQueryStrict)
import Moongle.Query.Parser (text2NmQuery, text2Query, text2TyQuery)
import Moongle.Query.Syntax
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "buildTsQuery" $ do
    it "adds prefix search for tokens ending with dot" $ do
      let tokens = map T.pack ["sym,foo.", "attr,async."]
      buildTsQuery tokens `shouldBe` T.pack "'sym,foo.':* & 'attr,async.':*"

    it "keeps exact match for strict builder" $ do
      let tokens = map T.pack ["sym,foo.", "plain"]
      buildTsQueryStrict tokens `shouldBe` T.pack "'sym,foo.' & 'plain'"

    it "escapes single quotes" $ do
      let tokens = [T.pack "sym,fo'o."]
      buildTsQuery tokens `shouldBe` T.pack "'sym,fo''o.':*"

  describe "query parser" $ do
    it "parses async type query with package filter" $ do
      let input = "+pkg:core async (Int) -> String"
      case text2TyQuery (T.pack input) of
        Right (TyQuery modifiers tyQuery) -> do
          includePackages modifiers `shouldBe` [["core"]]
          excludePackages modifiers `shouldBe` []
          queryParams tyQuery `shouldSatisfy` \case
            [AnonParam _ (TName Nothing (TCon "Int" []))] -> True
            _ -> False
          queryReturnTy tyQuery `shouldBe` TName Nothing (TCon "String" [])
          queryEff tyQuery `shouldBe` [EffAsync, EffException NoAraise]
        other -> expectationFailure $ "unexpected parse result: " <> show other

    it "parses module-qualified name query" $ do
      let input = "@btree/btree.insert"
      case text2NmQuery (T.pack input) of
        Right (NmQuery modifiers NameQuery{queryModulePath = Just path, queryName = TCon "insert" []}) -> do
          includePackages modifiers `shouldBe` []
          excludePackages modifiers `shouldBe` []
          path `shouldBe` TPath ["btree"] "btree"
        other -> expectationFailure $ "unexpected parse result: " <> show other

    it "parses mixed query via dispatch" $ do
      let input = "pkg:core map"
      case text2Query (T.pack input) of
        Right (NmQuery mods _) -> includePackages mods `shouldBe` [["core"]]
        other -> expectationFailure $ "unexpected parse result: " <> show other

    it "fails on empty package selector" $ do
      text2Query (T.pack "+pkg:/ foo") `shouldSatisfy` isLeft
