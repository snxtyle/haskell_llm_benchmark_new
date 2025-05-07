{-# LANGUAGE OverloadedStrings #-}

module Sgf (parseSgf) where

import           Control.Applicative        ((<|>), many)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tree                  (Tree (..))
import           Text.Parsec                (Parsec, anyChar, char, eof,
                                             many1, manyTill, optionMaybe,
                                             parse, satisfy, sepBy, try, (<|>))
import           Text.Parsec.Text           (Parser)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | Parse an SGF string into a tree of nodes with properties.
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf input = case parse sgfTreeParser "" input of
  Left _  -> Nothing
  Right t -> Just t

-- Parsers

-- SGF tree parser: a tree is a sequence of nodes enclosed in parentheses,
-- possibly with child trees (variations).
sgfTreeParser :: Parser SgfTree
sgfTreeParser = do
  _ <- char '('
  nodes <- many1 nodeParser
  children <- many sgfTreeParser
  _ <- char ')'
  -- The root node is the first node, children are variations of the last node
  -- But SGF trees are trees of nodes, so we build a tree with the first node as root,
  -- and the rest nodes as a chain of single children, then attach children as variations
  -- Actually, the SGF tree is a tree of nodes, where each node can have multiple children (variations).
  -- The nodes list is a sequence of nodes connected by single-child edges.
  -- The children list are variations branching from the last node.
  -- So we build a chain of nodes from nodes list, then attach children as children of the last node.
  let tree = chainNodes nodes children
  return tree

-- Build a chain of nodes from a list of nodes and attach children as variations to the last node
chainNodes :: [SgfNode] -> [SgfTree] -> SgfTree
chainNodes [] _ = error "chainNodes: empty node list"
chainNodes [n] cs = Node n cs
chainNodes (n:ns) cs = Node n [chainNodes ns cs]

-- Parse a node: a semicolon followed by zero or more properties
nodeParser :: Parser SgfNode
nodeParser = do
  _ <- char ';'
  props <- many propertyParser
  -- keys must be unique, so we combine them into a map
  let propMap = Map.fromList props
  return propMap

-- Parse a property: a key (uppercase letters) followed by one or more values
propertyParser :: Parser (Text, [Text])
propertyParser = do
  key <- propertyIdent
  values <- many1 propertyValue
  return (key, values)

-- Parse a property identifier: one or more uppercase letters
propertyIdent :: Parser Text
propertyIdent = do
  cs <- many1 (satisfy (\c -> c >= 'A' && c <= 'Z'))
  return (T.pack cs)

-- Parse a property value: text enclosed in square brackets, with escaping
propertyValue :: Parser Text
propertyValue = do
  _ <- char '['
  content <- manyTill propertyValueChar (char ']')
  return (T.pack content)

-- Parse a character inside a property value, handling escapes
propertyValueChar :: Parser Char
propertyValueChar = escapedChar <|> normalChar
  where
    escapedChar = do
      _ <- char '\\'
      c <- anyChar
      return c
    normalChar = satisfy (\c -> c /= ']' && c /= '\\')

