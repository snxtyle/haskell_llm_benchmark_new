module Sgf (parseSgf) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text (Parser)

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
  tree <- gameTree
  eof
  return tree

gameTree :: Parser SgfTree
gameTree = do
  _ <- char '('
  nodes <- many1 node
  children <- many gameTree
  _ <- char ')'
  return $ buildTree nodes children

buildTree :: [SgfNode] -> [SgfTree] -> SgfTree
buildTree [] _ = error "Empty node list"
buildTree [n] children = Node n children
buildTree (n:ns) children = Node n [buildTree ns children]

node :: Parser SgfNode
node = do
  _ <- char ';'
  props <- many property
  return $ Map.fromList props

property :: Parser (Text, [Text])
property = do
  key <- propIdent
  values <- many1 propValue
  return (key, values)

propIdent :: Parser Text
propIdent = do
  chars <- many1 upper
  return $ T.pack chars

propValue :: Parser Text
propValue = do
  _ <- char '['
  value <- many valueChar
  _ <- char ']'
  return $ T.pack value

valueChar :: Parser Char
valueChar = escapedChar <|> normalChar

escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  c <- anyChar
  case c of
    '\n' -> return ' '  -- Escaped newline becomes space
    't'  -> return '\t' -- Escaped t becomes tab
    'n'  -> return '\n' -- Escaped n becomes newline
    _    -> return c    -- Any other escaped char is literal

normalChar :: Parser Char
normalChar = noneOf "\\]"
