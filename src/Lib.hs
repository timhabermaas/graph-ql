{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( parseGraphQL
    -- TODO move these to some internal type module which can be used from the tests
    , GQLDocument (..)
    , GQLOperationDefinition (..)
    , GQLSelection (..)
    , GQLVariable (..)
    , GQLType (..)
    , GQLBaseType (..)
    ) where

import Data.Text
import Text.Parsec


type GraphQLParser a = Parsec Text () a

type Name = String
data GQLSelection = GQLField Name GQLSelectionList deriving (Eq, Show)--GQLSelection Name GQLSelectionList deriving (Eq, Show)
type GQLSelectionList = [GQLSelection]
data GQLBaseType = GQLNamedType String | GQLListType String deriving (Eq, Show)
data GQLType = GQLType GQLBaseType | GQLNullableType GQLBaseType deriving (Eq, Show)
-- TODO int, float and stuff...
type GQLDefaultValue = String
data GQLVariable = GQLVariable Name GQLType (Maybe GQLDefaultValue) deriving (Eq, Show)
-- FIXME This allows a query to have variables without being named which
--       is actually not allowed by the specification.
data GQLOperationDefinition = GQLQuery (Maybe Name) [GQLVariable] GQLSelectionList | GQLCommand | Nope deriving (Eq, Show)
type GQLDocument = GQLOperationDefinition

ignoredChars =
    skipMany ignoredChar
  where
    -- TODO there are actually more characters to ignore
    --      according to the spec
    ignoredChar = space <|> endOfLine

name :: GraphQLParser Name
name = do
    c <- firstCharacter
    cs <- remainingCharacters
    return (c:cs)
  where
    firstCharacter = oneOf ['a'..'z'] <|> oneOf ['A'..'Z'] <|> char '_'
    remainingCharacters = many $ firstCharacter <|> oneOf ['0'..'9']

field :: GraphQLParser GQLSelection
field = do
    ignoredChars
    name <- name
    ignoredChars
    selectionList <- (try selectionSetParser <|> return [])
    ignoredChars
    return $ GQLField name selectionList

selectionSetParser :: Parsec Text () GQLSelectionList
selectionSetParser = do
    ignoredChars
    char '{'
    ignoredChars
    fields <- many1 field
    ignoredChars
    char '}'
    ignoredChars
    return fields

variableDefinitionParser :: Parsec Text () [GQLVariable]
variableDefinitionParser = do
    ignoredChars
    char '('
    ignoredChars
    variables <- many1 variableParser
    ignoredChars
    char ')'
    ignoredChars
    return variables
  where
    variableParser = do
      ignoredChars
      char '$'
      variableName <- name
      ignoredChars
      char ':'
      ignoredChars
      type' <- typeParser
      ignoredChars
      -- TODO: default value
      return $ GQLVariable variableName type' Nothing
    typeParser = do
      type' <- listType <|> namedType
      ignoredChars
      -- TODO: Too cryptic?
      try (GQLNullableType type' <$ char '!') <|> (return $ GQLType type')
    namedType = do
      name <- name
      return $ GQLNamedType name
    listType = do
      char '['
      ignoredChars
      name <- name
      ignoredChars
      char ']'
      return $ GQLListType name


operationDefinitionParser :: Parsec Text () GQLDocument
operationDefinitionParser = do
    ignoredChars
    try shortSyntax <|> longSyntax
  where
    shortSyntax = do
      fields <- selectionSetParser
      return $ GQLQuery Nothing [] fields
    longSyntax = do
      string "query"
      ignoredChars
      name <- name
      ignoredChars
      variables <- try variableDefinitionParser <|> return []
      ignoredChars
      fields <- selectionSetParser
      return $ GQLQuery (Just name) variables fields


parseGraphQL :: Text -> Either ParseError GQLDocument
parseGraphQL text =
    myParse operationDefinitionParser text
  where
    myParse rule text = parse rule "" text
