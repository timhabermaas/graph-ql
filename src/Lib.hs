{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( parseGraphQL
    , GQLDocument (..)
    , GQLSelection (..)
    ) where

import Data.Text
import Text.Parsec


type GraphQLParser a = Parsec Text () a

type Identifier = String
data GQLSelection = GQLSelection Identifier deriving (Eq, Show)
type GQLSelectionList = [GQLSelection]
data GQLDocument = GQLQuery GQLSelectionList | GQLCommand | Nope deriving (Eq, Show)

ignoredChars =
    skipMany ignoredChar
  where
    ignoredChar = space <|> endOfLine

identifier :: GraphQLParser Identifier
identifier = do
    c <- firstCharacter
    cs <- remainingCharacters
    return (c:cs)
  where
    firstCharacter = oneOf ['a'..'z'] <|> oneOf ['A'..'Z'] <|> char '_'
    remainingCharacters = many $ firstCharacter <|> oneOf ['0'..'9']

field :: GraphQLParser GQLSelection
field = do
    ignoredChars
    name <- identifier
    ignoredChars
    return $ GQLSelection name

documentParser :: Parsec Text () GQLDocument
documentParser = do
    char '{'
    ignoredChars
    fields <- many1 field
    ignoredChars
    char '}'
    return $ GQLQuery fields


parseGraphQL :: Text -> Maybe GQLDocument
parseGraphQL text =
    case myParse documentParser text of
        Right r -> Just r
        Left _ -> Nothing
  where
    myParse rule text = parse rule "" text
