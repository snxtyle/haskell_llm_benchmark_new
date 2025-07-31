module Sgf (parseSgf) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text

type SgfNode = Map Text [Text]
type SgfTree = Tree SgfNode

parseSgf :: Text -> Maybe SgfTree
parseSgf input = case parse sgfParser "" input of
    Left _  -> Nothing
    Right tree -> Just tree

sgfParser :: Parser SgfTree
sgfParser = char '(' *> gameTree <* char ')'

gameTree :: Parser SgfTree
gameTree = do
    node <- nodeParser
    variations <- many (char '(' *> gameTree <* char ')')
    case variations of
        [] -> return $ Node node []
        _  -> return $ Node node variations

nodeParser :: Parser SgfNode
nodeParser = do
    char ';'
    props <- many propertyParser
    return $ Map.fromList props

propertyParser :: Parser (Text, [Text])
propertyParser = do
    key <- upperIdentifier
    values <- many1 propValue
    return (key, values)

upperIdentifier :: Parser Text
upperIdentifier = do
    first <- upper
    rest <- many (upper <|> digit)
    return $ Text.pack (first:rest)

propValue :: Parser Text
propValue = do
    char '['
    value <- many propValueChar
    char ']'
    return $ Text.pack value

propValueChar :: Parser Char
propValueChar = 
    try (char '\\' *> (char ']' <|> char '\\')) <|>
    noneOf "]"
