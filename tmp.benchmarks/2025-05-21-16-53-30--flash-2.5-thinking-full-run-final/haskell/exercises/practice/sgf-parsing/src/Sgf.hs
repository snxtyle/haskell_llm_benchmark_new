module Sgf (parseSgf) where

import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import Data.Tree (Tree(Node))
import Text.Parsec
import Text.Parsec.Text (Parser)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- Parser for an SGF value (content inside brackets)
-- e.g., "[value]"
sgfValueP :: Parser Text
sgfValueP = char '[' *> (pack <$> many (noneOf "]")) <* char ']'

-- Parser for an SGF property (KEY[value1][value2]...)
-- e.g., "FF[4]" or "AB[aa][ab]"
sgfPropertyP :: Parser (Text, [Text])
sgfPropertyP = do
    key <- pack <$> many1 upper
    values <- many1 sgfValueP
    return (key, values)

-- Parser for an SgfNode (semicolon followed by properties)
-- e.g., ";FF[4]C[root]SZ[19]"
sgfNodeP :: Parser SgfNode
sgfNodeP = char ';' *> (fromList <$> many sgfPropertyP)

-- Forward declaration for mutual recursion
gameTreeP :: Parser (Tree SgfNode)
gameTreeP = char '(' *> nodeAndChildrenP <* char ')'

-- Parses a node and its children (which can be sequential or variations)
-- This is the core recursive parser that builds the Tree structure.
nodeAndChildrenP :: Parser (Tree SgfNode)
nodeAndChildrenP = do
    rootNode <- sgfNodeP
    -- Children can be either a sequence of nodes (e.g., `;N2;N3`)
    -- OR multiple game trees (e.g., `(;GT1)(;GT2)`).
    -- The `many` combinator will try to parse `gameTreeP` first.
    -- If `gameTreeP` fails (e.g., no opening parenthesis '('),
    -- it will then try `sequentialChildTreeP` (e.g., if it sees a semicolon ';').
    -- `try` is essential for `gameTreeP` because it consumes `(` and might fail later,
    -- requiring backtracking to try `sequentialChildTreeP`.
    subForest <- many (try gameTreeP <|> sequentialChildTreeP)
    return $ Node rootNode subForest

-- Parses a sequential child tree (e.g., `;N2;N3` forms one `Tree N2 (Tree N3 [])`)
-- This parser is essentially the same as `nodeAndChildrenP` because a sequential
-- child node itself can have children (either sequential or variations).
-- The `sgfNodeP` inside `nodeAndChildrenP` will consume the leading semicolon.
sequentialChildTreeP :: Parser (Tree SgfNode)
sequentialChildTreeP = nodeAndChildrenP

-- The main parse function
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf = case parse gameTreeP "" sgf of
    Left _ -> Nothing
    Right tree -> Just tree
