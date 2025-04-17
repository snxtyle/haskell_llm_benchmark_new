module Sgf (parseSgf) where

import           Control.Applicative            (many, (<|>))
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Tree                      (Tree(Node))
import           Text.Parsec
import           Text.Parsec.String             (Parser)

-- | Parse an SGF document into a Tree of property maps.
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf =
  case parse sgfTreeParser "" (T.unpack sgf) of
    Left _     -> Nothing
    Right tree -> Just tree

-- Top‐level parser: skip any leading spaces, parse a tree, then eof
sgfTreeParser :: Parser (Tree (Map Text [Text]))
sgfTreeParser = do
  skipMany space
  t <- parseTree
  eof
  return t

-- Recursive tree parser: '(' Sequence { Tree } ')'
parseTree :: Parser (Tree (Map Text [Text]))
parseTree = do
  char '('
  nodes    <- many1 parseNode
  subTrees <- many parseTree
  char ')'
  return (assemble nodes subTrees)

-- Build a single Tree: chain the sequence of nodes, attaching subTrees to the last node
assemble :: [Map Text [Text]] -> [Tree (Map Text [Text])] -> Tree (Map Text [Text])
assemble nodes subTrees =
  let revNodes = reverse nodes
      lastMap  = head revNodes
      -- start with the last node, attaching any variations as its children
      baseTree = Node lastMap subTrees
      -- then wrap each preceding node as a single-child parent
      wrap acc []       = acc
      wrap acc (m:ms)   = wrap (Node m [acc]) ms
  in wrap baseTree (tail revNodes)

-- Parse a single node: ';' followed by zero or more properties
parseNode :: Parser (Map Text [Text])
parseNode = do
  char ';'
  props <- many parseProp
  return (Map.fromList props)

-- Parse one property: KEY Value+
parseProp :: Parser (Text, [Text])
parseProp = do
  key    <- many1 upper
  values <- many1 parseValue
  return (T.pack key, map T.pack values)

-- Parse one value: '[' text ']'
parseValue :: Parser String
parseValue = between (char '[') (char ']') (many valueChar)

-- Inside a value, allow escaped chars or anything but ']' and '\'
valueChar :: Parser Char
valueChar = escaped <|> noneOf "\\]"

escaped :: Parser Char
escaped = do
  char '\\'
  anyChar
