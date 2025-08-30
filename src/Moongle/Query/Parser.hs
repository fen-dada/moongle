{-# LANGUAGE OverloadedStrings #-}

module Moongle.Query.Parser (pQuery, text2Query) where

import Data.Functor
import Data.Text qualified as T
import Data.Text.Lazy (fromStrict)
import Language.Moonbit.Lexer
import Language.Moonbit.Mbti.Parser
import Language.Moonbit.Mbti.Syntax
import Moongle.Query.Syntax
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

-- >>> parse pTyQuery "" "async [K : Compare, V](K, V) -> Int raise E"
-- Right (TyQuery (TypeQuery {queryTyParams = [(TCon "K" [],[CTrait (TTrait Nothing "Compare")]),(TCon "V" [],[])], queryParams = [AnonParam False (TName Nothing (TCon "K" [])),AnonParam False (TName Nothing (TCon "V" []))], queryReturnTy = TName Nothing (TCon "Int" []), queryEff = [EffAsync,EffException (Araise (TName Nothing (TCon "E" [])))]}))

-- >>> parse pTyQuery "" "[K, V](K, V) -> Int"
-- Right (TyQuery (TypeQuery {queryTyParams = [(TCon "K" [],[]),(TCon "V" [],[])], queryParams = [AnonParam False (TName Nothing (TCon "K" [])),AnonParam False (TName Nothing (TCon "V" []))], queryReturnTy = TName Nothing (TCon "Int" []), queryEff = [EffException NoAraise]}))

-- >>> parse pTyQuery "" "(Int) -> String"
-- Right (TyQuery (TypeQuery {queryTyParams = [], queryParams = [AnonParam False (TName Nothing (TCon "Int" []))], queryReturnTy = TName Nothing (TCon "String" []), queryEff = [EffException NoAraise]}))

pTyQuery :: Parser Query
pTyQuery = do
  isAsync <- option False (reserved RWAsync $> True)
  typs <- option [] pTyParams
  params <- parens (commaSep pParam) -- 5) return arrow + return type
  reservedOp OpArrow
  retTy <- pType
  effEx <- option NoAraise pEffectException
  let effects = [EffAsync | isAsync] ++ [EffException effEx]
  return $ TyQuery (TypeQuery typs params retTy effects)

-- >>> parse pNmQuery "" "@btree/btree.insert"
-- Right (NmQuery (NameQuery {queryModulePath = Just (TPath ["btree"] "btree"), queryName = TCon "insert" []}))

pNmQuery :: Parser Query
pNmQuery = do
  mpath <- optionMaybe (pTPath <* reservedOp OpDot)
  NmQuery . NameQuery mpath <$> pTCon

pQuery :: Parser Query
pQuery = try pTyQuery <|> pNmQuery

text2Query :: T.Text -> Either ParseError Query
text2Query = parse pQuery "" . fromStrict
