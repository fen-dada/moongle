module Moongle.Query.Syntax
  ( Query (..),
    TypeQuery (..),
    NameQuery (..),
    QueryModifiers (..),
    emptyModifiers,
    queryModifiers,
  )
where

import Data.Text (Text)
import Language.Moonbit.Mbti.Syntax

data Query
  = TyQuery QueryModifiers TypeQuery
  | NmQuery QueryModifiers NameQuery
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

data QueryModifiers = QueryModifiers
  { includePackages :: [[Text]],
    excludePackages :: [[Text]]
  }
  deriving (Show, Eq)

emptyModifiers :: QueryModifiers
emptyModifiers = QueryModifiers [] []

queryModifiers :: Query -> QueryModifiers
queryModifiers (TyQuery modifiers _) = modifiers
queryModifiers (NmQuery modifiers _) = modifiers
