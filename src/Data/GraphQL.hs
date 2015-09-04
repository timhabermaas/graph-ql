{-# LANGUAGE FlexibleContexts #-}

module Data.GraphQL
    ( parseGraphQL
    ) where

import Data.Text
import Text.Parsec

import Data.GraphQL.Types


type GraphQLParser a = Parsec Text () a


ignoredChars =
    skipMany ignoredChar
  where
    -- TODO there are actually more characters to ignore
    --      according to the spec
    ignoredChar = space <|> endOfLine

name :: GraphQLParser GQLName
name = do
    c <- firstCharacter
    cs <- remainingCharacters
    return (c:cs)
  where
    firstCharacter = oneOf ['a'..'z'] <|> oneOf ['A'..'Z'] <|> char '_'
    remainingCharacters = many $ firstCharacter <|> oneOf ['0'..'9']

valueParser = do
    ignoredChars *> (try object <|> try list <|> bool <|> int) <* ignoredChars

bool :: GraphQLParser GQLValue
bool = do
    GQLBooleanValue <$> ((True <$ string "true") <|> (False <$ string "false"))

int :: GraphQLParser GQLValue
int = do
    signF <- try ((*(-1)) <$ char '-') <|> return id
    number <- try digits <|> zero
    return $ GQLIntValue $ signF $ read $ number
  where
    digits = (:) <$> nonZeroDigit <*> many digit
    nonZeroDigit = oneOf ['1'..'9']
    zero = "0" <$ char '0'

list :: GraphQLParser GQLValue
list = do
    char '['
    ignoredChars
    values <- many valueParser
    ignoredChars
    char ']'
    return $ GQLListValue values

object :: GraphQLParser GQLValue
object = do
    char '{'
    ignoredChars
    objectFields <- many objectField
    ignoredChars
    char '}'
    return $ GQLObjectValue objectFields
  where
    objectField = do
      ignoredChars
      name <- name
      ignoredChars
      char ':'
      ignoredChars
      content <- valueParser
      ignoredChars
      return $ GQLObjectField name content


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
      defaultValue <- try (Just <$> defaultValueParser) <|> (return Nothing)
      ignoredChars
      -- TODO: default value
      return $ GQLVariable variableName type' defaultValue
    defaultValueParser = do
      char '='
      ignoredChars
      v <- valueParser
      return v


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
