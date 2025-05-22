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
    Left _ -> Nothing
    Right tree -> Just tree

-- Parser for the entire SGF tree
sgfParser :: Parser SgfTree
sgfParser = do
    _ <- char '('
    tree <- treeParser
    _ <- char ')'
    eof
    return tree

-- Parser for a tree (node with potential children)
treeParser :: Parser SgfTree
treeParser = do
    nodes <- many1 nodeParser
    variations <- many variationParser
    return $ buildTree nodes variations

-- Build a tree from a list of nodes and variations
buildTree :: [SgfNode] -> [SgfTree] -> SgfTree
buildTree [] _ = error "No nodes found"
buildTree [node] variations = Node node variations
buildTree (node:rest) variations = Node node [buildTree rest variations]

-- Parser for a variation (a tree in parentheses)
variationParser :: Parser SgfTree
variationParser = do
    _ <- char '('
    tree <- treeParser
    _ <- char ')'
    return tree

-- Parser for a single node
nodeParser :: Parser SgfNode
nodeParser = do
    _ <- char ';'
    props <- many propertyParser
    return $ Map.fromListWith (++) props

-- Parser for a property (key with one or more values)
propertyParser :: Parser (Text, [Text])
propertyParser = do
    key <- many1 upper
    values <- many1 valueParser
    return (T.pack key, values)

-- Parser for a property value
valueParser :: Parser Text
valueParser = do
    _ <- char '['
    content <- many valueChar
    _ <- char ']'
    return $ T.pack content

-- Parser for characters inside a property value
valueChar :: Parser Char
valueChar = escapeChar <|> whitespaceChar <|> noneOf "]"

-- Parser for whitespace characters (convert to space)
whitespaceChar :: Parser Char
whitespaceChar = do
    c <- oneOf " \t\n\r"
    return ' '

-- Parser for escape sequences
escapeChar :: Parser Char
escapeChar = do
    _ <- char '\\'
    c <- anyChar
    case c of
        ']' -> return ']'
        '\\' -> return '\\'
        _ -> return c
