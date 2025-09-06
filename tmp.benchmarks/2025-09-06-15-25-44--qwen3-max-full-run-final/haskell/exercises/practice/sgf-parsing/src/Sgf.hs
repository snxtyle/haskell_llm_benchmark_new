module Sgf (parseSgf) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(Node))
import Text.Parsec
import Text.Parsec.Text (Parser)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf = case parse sgfParser "" sgf of
  Left _ -> Nothing
  Right tree -> Just tree

-- Main parser for SGF format
sgfParser :: Parser (Tree (Map Text [Text]))
sgfParser = do
  optional whitespace
  char '('
  optional whitespace
  char ';'
  rootNode <- propertyList
  children <- many (variation <|> node)
  char ')'
  return $ Node rootNode children

-- Parse a single node (starting with semicolon)
node :: Parser (Tree (Map Text [Text]))
node = do
  optional whitespace
  char ';'
  props <- propertyList
  children <- many (variation <|> node)
  return $ Node props children

-- Parse a variation (subtree in parentheses)
variation :: Parser (Tree (Map Text [Text]))
variation = do
  optional whitespace
  char '('
  optional whitespace
  child <- node
  many whitespace
  char ')'
  return child

-- Parse a property list for a node
propertyList :: Parser (Map Text [Text])
propertyList = do
  props <- many property
  return $ Map.fromListWith (++) [(k, [v]) | (k, v) <- props]

-- Parse a single property (key followed by one or more values in brackets)
property :: Parser (Text, Text)
property = do
  key <- many1 (noneOf "[];()")
  values <- many1 (bracketedText)
  return (T.pack key, T.concat values)

-- Parse text within square brackets, handling escaped characters
bracketedText :: Parser Text
bracketedText = do
  char '['
  content <- many (escapedChar <|> noneOf "]")
  char ']'
  return $ T.pack content

-- Handle escaped characters within bracketed text
escapedChar :: Parser Char
escapedChar = do
  char '\\'
  c <- anyChar
  return $ case c of
    'n' -> '\n'
    't' -> '\t'
    '\\' -> '\\'
    ']' -> ']'
    _ -> c

-- Parse optional whitespace
whitespace :: Parser ()
whitespace = skipMany (oneOf " \t\n\r") >> return ()
