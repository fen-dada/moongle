{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}
module Moongle.Query.Parser where

import Data.Functor
import Language.Moonbit.Lexer
import Language.Moonbit.Mbti.Parser
import Language.Moonbit.Mbti.Syntax
import Moongle.Query.Syntax
import Text.Parsec
import Text.Parsec.Text.Lazy (Parser)

-- >>> parse pTyQuery "" "async [K : Compare, V](K, V) -> Int raise E"
-- Right (TyQuery (TypeQuery {queryTyParams = [(TCon "K" [],[CTrait (TTrait Nothing "Compare")]),(TCon "V" [],[])], queryParams = [AnonParam False (TName Nothing (TCon "K" [])),AnonParam False (TName Nothing (TCon "V" []))], queryReturnTy = TName Nothing (TCon "Int" []), queryEff = [EffAsync,EffException (Araise (TName Nothing (TCon "E" [])))]}))

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
-- Right (NmQuery (Just (TPath ["btree"] "btree")) (TCon "insert" []))

pNmQuery :: Parser Query
pNmQuery = do
  mpath <- optionMaybe (pTPath <* reservedOp OpDot)
  NmQuery . NameQuery mpath <$> pTCon

pQuery :: Parser Query
pQuery = try pTyQuery <|> pNmQuery
