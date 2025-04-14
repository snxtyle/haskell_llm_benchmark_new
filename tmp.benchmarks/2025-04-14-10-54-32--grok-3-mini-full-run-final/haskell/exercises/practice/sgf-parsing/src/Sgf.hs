module Sgf (parseSgf) where

import Data.Map  (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))  -- Updated to import Tree(..) as per error message
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, noneOf, char, string)
import Text.Parsec.Combinator (many1, sepBy1, between)

-- | A tree of nodes.
type SgfTree = Tree (Map Text [Text])

-- | A node is a property list.
type SgfNode = Map Text [Text]

-- Parser for a single value (e.g., inside brackets, treated as text)
valueParser :: Parser Text
valueParser = T.pack <$> between (char '[') (char ']') (many1 (noneOf ['\n', ']']))

-- Parser for a property (e.g., "FF[4]" or "AB[aa][ab]")
propertyParser :: Parser (Text, [Text])
propertyParser = do
    key <- T.pack <$> many1 (oneOf ['A'..'Z'])  -- Keys are uppercase letters
    values <- many1 valueParser  -- One or more values
    return (key, values)

-- Parser for a node (a list of properties)
nodeParser :: Parser SgfNode
nodeParser = do
    properties <- sepBy1 propertyParser (char ';')  -- Properties separated by ';'
    let nodeMap = Map.fromListWith (++) properties  -- Combine into a Map, allowing multiple values per key
    return nodeMap

-- Parser for a tree (recursive for subtrees)
treeParser :: Parser SgfTree
treeParser = do
    char '('  -- Start of tree
    node <- nodeParser  -- Parse the node
    subtrees <- many treeParser  -- Parse zero or more subtrees (variations)
    char ')'  -- End of tree
    return $ Node node subtrees  -- Build the tree

-- Main parse function
parseSgf :: Text -> Maybe SgfTree
parseSgf sgfText =
    case parse (spaces >> treeParser <* eof) "" (T.unpack sgfText) of  -- Parse the entire string
        Right tree -> Just tree  -- Successfully parsed
        Left _     -> Nothing    -- Parsing failed
