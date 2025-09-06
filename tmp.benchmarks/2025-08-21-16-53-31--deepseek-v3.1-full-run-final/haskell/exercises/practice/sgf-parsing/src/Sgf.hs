module Sgf (parseSgf) where

import Data.Map (Map, fromList)
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
parseSgf sgf = case parse gameTree "" sgf of
    Right tree -> Just tree
    Left _ -> Nothing

-- Parse a complete game tree
gameTree :: Parser SgfTree
gameTree = do
    _ <- char '('
    nodes <- many1 nodeParser
    subtrees <- many gameTree
    _ <- char ')'
    
    -- Build the tree: first node is root, subsequent nodes form a chain
    let buildTree [node] = Node node subtrees
        buildTree (node:nodes) = Node node [buildTree nodes]
    
    return (buildTree nodes)

-- Parse a single node
nodeParser :: Parser SgfNode
nodeParser = do
    _ <- char ';'
    props <- many propertyParser
    return (fromList props)

-- Parse a property (key and values)
propertyParser :: Parser (Text, [Text])
propertyParser = do
    key <- many1 upper
    values <- many1 valueParser
    return (T.pack key, map T.pack values)

-- Parse a single value (enclosed in brackets)
valueParser :: Parser String
valueParser = do
    _ <- char '['
    content <- many (escapedChar <|> noneOf "]")
    _ <- char ']'
    return content

-- Parse escaped characters
escapedChar :: Parser Char
escapedChar = do
    _ <- char '\\'
    c <- anyChar
    return c
