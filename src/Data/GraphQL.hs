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
    ignoredChar = space <|> endOfLine <|> char ','

name :: GraphQLParser GQLName
name = do
    (:) <$> firstCharacter <*> remainingCharacters
  where
    firstCharacter = oneOf ['a'..'z'] <|> oneOf ['A'..'Z'] <|> char '_'
    remainingCharacters = many $ firstCharacter <|> oneOf ['0'..'9']

valueParser = do
    ignoredChars *> (try object
                <|> try list
                <|> GQLVariableValue <$> try variable
                <|> try stringValue
                <|> bool
                <|> try enum
                <|> try float
                <|> int)
    <* ignoredChars

stringValue :: GraphQLParser GQLValue
stringValue = do
    char '"'
    chars <- many sourceCharacter
    char '"'
    return $ GQLStringValue chars
  where
    -- TODO unicode support missing
    sourceCharacter = noneOf ['"', '\\', '\n']

bool :: GraphQLParser GQLValue
bool = do
      GQLBooleanValue True <$ string "true"
  <|> GQLBooleanValue False <$ string "false"


variable :: GraphQLParser GQLVariable
variable = char '$' *> (GQLVariable <$> name)

int :: GraphQLParser GQLValue
int = do
    signF <- try ((*(-1)) <$ char '-') <|> try (id <$ char '+') <|> return id
    number <- try digits <|> zero
    return $ GQLIntValue $ signF $ read $ number
  where
    digits = (:) <$> nonZeroDigit <*> many digit
    nonZeroDigit = oneOf ['1'..'9']
    zero = "0" <$ char '0'

float :: GraphQLParser GQLValue
-- TODO exponential missing; crappy implementation
float = do
    GQLIntValue intPart <- int
    f <- fractionalPart
    return $ GQLFloatValue $ read (show intPart ++ "." ++ f)
  where
    fractionalPart = do
      char '.'
      many (oneOf ['0'..'9'])


enum :: GraphQLParser GQLValue
enum = GQLEnumValue <$> name

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

variableDefinitionParser :: Parsec Text () [GQLVariableDefinition]
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
      return $ GQLVariableDefinition (GQLVariable variableName) type' defaultValue
    defaultValueParser = do
      char '='
      ignoredChars
      v <- valueParser
      return v


    typeParser = do
      type' <- listType <|> namedType
      ignoredChars
      -- TODO: Too cryptic?
      try (GQLNonNullType type' <$ char '!') <|> (return $ GQLType type')
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
