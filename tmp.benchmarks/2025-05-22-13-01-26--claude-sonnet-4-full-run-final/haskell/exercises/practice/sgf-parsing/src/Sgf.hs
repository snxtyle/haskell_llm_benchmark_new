module Sgf (parseSgf) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf = case parse sgfParser "" sgf of
  Left _  -> Nothing
  Right tree -> Just tree

sgfParser :: Parser SgfTree
sgfParser = do
  _ <- char '('
  tree <- sgfTree
  _ <- char ')'
  eof
  return tree

sgfTree :: Parser SgfTree
sgfTree = do
  _ <- char ';'
  node <- sgfNode
  children <- many sgfChild
  return $ Node node children

sgfChild :: Parser SgfTree
sgfChild = choice
  [ do _ <- char '('
       tree <- sgfTree
       _ <- char ')'
       return tree
  , sgfTree
  ]

sgfNode :: Parser SgfNode
sgfNode = do
  properties <- many sgfProperty
  return $ Map.fromList properties

sgfProperty :: Parser (Text, [Text])
sgfProperty = do
  key <- sgfKey
  values <- many1 sgfValue
  return (key, values)

sgfKey :: Parser Text
sgfKey = do
  key <- many1 upper
  return $ T.pack key

sgfValue :: Parser Text
sgfValue = do
  _ <- char '['
  value <- sgfValueContent
  _ <- char ']'
  return value

sgfValueContent :: Parser Text
sgfValueContent = do
  content <- many sgfChar
  return $ T.pack content

sgfChar :: Parser Char
sgfChar = escapedChar <|> normalChar
  where
    escapedChar = do
      _ <- char '\\'
      c <- anyChar
      return c
    normalChar = noneOf "]"
