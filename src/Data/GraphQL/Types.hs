module Data.GraphQL.Types
    ( GQLDocument (..)
    , GQLOperationDefinition (..)
    , GQLSelection (..)
    , GQLVariable (..)
    , GQLType (..)
    , GQLBaseType (..)
    , GQLValue (..)
    , GQLObjectField (..)
    , GQLName
    , GQLSelectionList
    ) where

type GQLName = String
data GQLSelection = GQLField GQLName GQLSelectionList deriving (Eq, Show)--GQLSelection GQLName GQLSelectionList deriving (Eq, Show)
type GQLSelectionList = [GQLSelection]
data GQLBaseType = GQLNamedType String | GQLListType String deriving (Eq, Show)
data GQLType = GQLType GQLBaseType | GQLNullableType GQLBaseType deriving (Eq, Show)
-- TODO int, float and stuff...
type GQLDefaultValue = GQLValue
data GQLVariable = GQLVariable GQLName GQLType (Maybe GQLDefaultValue) deriving (Eq, Show)
-- FIXME This allows a query to have variables without being named which
--       is actually not allowed by the specification.
data GQLOperationDefinition = GQLQuery (Maybe GQLName) [GQLVariable] GQLSelectionList | GQLCommand | Nope deriving (Eq, Show)
type GQLDocument = GQLOperationDefinition

data GQLObjectField = GQLObjectField GQLName GQLValue deriving (Eq, Show)

data GQLValue =
    GQLIntValue Int
  | GQLFloatValue Float
  | GQLStringValue String
  | GQLBooleanValue Bool
  | GQLObjectValue [GQLObjectField]
  deriving (Eq, Show)
