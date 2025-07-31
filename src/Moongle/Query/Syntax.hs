module Moongle.Query.Syntax
  ( Query (..),
    TypeQuery (..),
  )
where

import Language.Moonbit.Mbti.Syntax

data Query
  = TyQuery TypeQuery
  | NmQuery (Maybe TPath) TCon
  deriving (Show, Eq)

data TypeQuery = TypeQuery
  { queryTyParams :: [(TCon, [Constraint])],
    queryParams :: [FnParam],
    queryReturnTy :: Type,
    queryEff :: [Effect]
  }
  deriving (Show, Eq)
