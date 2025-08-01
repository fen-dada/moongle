module Moongle.Query.Syntax
  ( Query (..),
    TypeQuery (..),
    NameQuery (..),
  )
where

import Language.Moonbit.Mbti.Syntax

data Query
  = TyQuery TypeQuery
  | NmQuery NameQuery
  deriving (Show, Eq)

data NameQuery = NameQuery
  { queryModulePath :: Maybe TPath,
    queryName :: TCon
  }
  deriving (Show, Eq)

data TypeQuery = TypeQuery
  { queryTyParams :: [(TCon, [Constraint])],
    queryParams :: [FnParam],
    queryReturnTy :: Type,
    queryEff :: [Effect]
  }
  deriving (Show, Eq)
