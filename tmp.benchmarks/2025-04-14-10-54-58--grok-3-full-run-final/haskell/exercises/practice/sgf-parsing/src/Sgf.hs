module Sgf (parseSgf) where

import Data.Map (Map, fromList)
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text (Parser)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf input = case parse sgfParser "" input of
    Left _  -> Nothing
    Right t -> Just t

sgfParser :: Parser SgfTree
sgfParser = do
    char '('
    tree <- treeParser
    char ')'
    return tree

treeParser :: Parser SgfTree
treeParser = do
    node <- nodeParser
    branches <- many branchParser
    return $ Node node branches

branchParser :: Parser SgfTree
branchParser = do
    char '('
    tree <- treeParser
    char ')'
    return tree

nodeParser :: Parser SgfNode
nodeParser = do
    char ';'
    props <- many propertyParser
    return $ fromList props

propertyParser :: Parser (Text, [Text])
propertyParser = do
    key <- many1 upper
    values <- many1 valueParser
    return (pack key, values)

valueParser :: Parser Text
valueParser = do
    char '['
    value <- many (noneOf "]")
    char ']'
    return $ pack value
