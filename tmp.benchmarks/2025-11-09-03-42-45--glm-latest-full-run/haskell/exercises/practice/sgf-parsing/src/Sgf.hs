module Sgf (parseSgf) where

import Control.Applicative ((<|>))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.ParserCombinators.Parsec
  ( CharParser,
    Parser,
    anyChar,
    between,
    char,
    eof,
    many,
    many1,
    noneOf,
    oneOf,
    parse,
    satisfy,
    try,
  )

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | Parses an SGF string into a tree of nodes.
-- Returns Nothing if the parsing fails.
parseSgf :: Text -> Maybe SgfTree
parseSgf text =
  case parse sgfFile "" (T.unpack text) of
    Left _ -> Nothing
    Right tree -> Just tree

-- | Top-level parser for an SGF file. Consumes the entire input.
sgfFile :: Parser SgfTree
sgfFile = do
  tree <- gameTree
  eof
  return tree

-- | Parses a GameTree, which is a sequence of nodes followed by variations (children).
-- GameTree = '(' Sequence GameTree* ')'
-- Sequence = Node+
gameTree :: Parser SgfTree
gameTree = do
  char '('
  nodes <- many1 (char ';' >> nodeParser)
  children <- many gameTree
  char ')'
  return $ buildTree nodes children

-- | Builds a tree from a list of nodes (representing a sequence) and a list of children.
-- The children are attached to the last node in the sequence.
buildTree :: [SgfNode] -> [SgfTree] -> SgfTree
buildTree [] _ = error "Cannot build tree from empty node list"
buildTree [n] children = Node n children
buildTree (n : ns) children = Node n [buildTree ns children]

-- | Parses a single node, which is a semicolon followed by properties.
nodeParser :: Parser SgfNode
nodeParser = do
  props <- many property
  return $ Map.fromListWith (++) props

-- | Parses a single property, which is an identifier and one or more values.
property :: Parser (Text, [Text])
property = do
  ident <- propIdent
  vals <- many1 propValue
  return (T.pack ident, map T.pack vals)

-- | Parses a property identifier (one or more uppercase letters).
propIdent :: Parser String
propIdent = many1 (satisfy isUpper)
  where
    isUpper c = c >= 'A' && c <= 'Z'

-- | Parses a single property value enclosed in square brackets.
propValue :: Parser String
propValue = between (char '[') (char ']') (many propValueChar)

-- | Parses a single character inside a property value, handling escapes.
propValueChar :: Parser Char
propValueChar = noneOf "\\]" <|> escapedChar
  where
    escapedChar = do
      char '\\'
      c <- oneOf "nt\\[]:"
      case c of
        'n' -> return '\n'
        't' -> return '\t'
        _ -> return c -- for \, [, ], :
