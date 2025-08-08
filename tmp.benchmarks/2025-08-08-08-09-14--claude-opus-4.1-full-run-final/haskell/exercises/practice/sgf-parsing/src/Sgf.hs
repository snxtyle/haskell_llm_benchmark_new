module Sgf (parseSgf) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text (Parser)
import Control.Applicative ((<|>))

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
    tree <- parseSequence
    _ <- char ')'
    eof
    return tree

-- Parse a sequence of nodes with possible variations
parseSequence :: Parser SgfTree
parseSequence = do
    nodes <- many1 parseNode
    variations <- many parseVariation
    return $ buildTree nodes variations

-- Build a tree from a list of nodes and variations
buildTree :: [SgfNode] -> [SgfTree] -> SgfTree
buildTree [] _ = error "Empty node list"
buildTree [node] variations = Node node variations
buildTree (node:rest) variations = Node node [buildTree rest variations]

-- Parse a variation (a tree in parentheses)
parseVariation :: Parser SgfTree
parseVariation = do
    _ <- char '('
    tree <- parseSequence
    _ <- char ')'
    return tree

-- Parse a single node
parseNode :: Parser SgfNode
parseNode = do
    _ <- char ';'
    properties <- many parseProperty
    return $ Map.fromListWith (++) properties

-- Parse a property (key with one or more values)
parseProperty :: Parser (Text, [Text])
parseProperty = do
    key <- parseKey
    values <- many1 parseValue
    return (T.pack key, map T.pack values)

-- Parse a property key (uppercase letters)
parseKey :: Parser String
parseKey = many1 upper

-- Parse a property value in brackets
parseValue :: Parser String
parseValue = do
    _ <- char '['
    content <- many parseValueChar
    _ <- char ']'
    return content

-- Parse characters inside a property value
parseValueChar :: Parser Char
parseValueChar = parseEscaped <|> noneOf "]"

-- Parse escaped characters
parseEscaped :: Parser Char
parseEscaped = do
    _ <- char '\\'
    c <- anyChar
    case c of
        ']' -> return ']'
        '\\' -> return '\\'
        ':' -> return ':'
        '\n' -> return ' '  -- Soft line break (converted to space)
        '\r' -> parseNewline -- Handle \r\n as soft line break
        _ -> return c -- Other escaped characters are kept as-is

-- Handle \r\n sequence after escape
parseNewline :: Parser Char
parseNewline = do
    _ <- optional (char '\n')
    return ' '  -- Soft line break
