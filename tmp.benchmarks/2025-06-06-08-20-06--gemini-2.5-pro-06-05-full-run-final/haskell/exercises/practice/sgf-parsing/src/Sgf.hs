module Sgf (parseSgf) where

import Data.Map (Map, fromListWith)
import Data.Text (Text, pack, unpack)
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.String (Parser)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | Main parsing function. It takes a Text, converts to String for parsec,
-- | and runs the parser.
parseSgf :: Text -> Maybe SgfTree
parseSgf sgf = case parse (sgfParser <* eof) "" (unpack sgf) of
    Left _  -> Nothing
    Right t -> Just t

-- | A SGF file contains a single game tree.
sgfParser :: Parser SgfTree
sgfParser = gameTreeParser

-- | A game tree is enclosed in parentheses and contains a sequence of nodes
-- | followed by zero or more variations (which are also game trees).
-- | e.g. (;C[root];B[aa](;W[ab])(;W[ac]))
gameTreeParser :: Parser SgfTree
gameTreeParser = between (char '(') (char ')') $ do
    nodes <- many1 nodeParser
    variations <- many gameTreeParser
    let (Node rootNode children) = buildTreeFromNodeList nodes
    return (Node rootNode (children ++ variations))

-- | Given a list of nodes, builds a linear tree where each node is a child
-- | of the previous one.
buildTreeFromNodeList :: [SgfNode] -> SgfTree
buildTreeFromNodeList [] = error "buildTreeFromNodeList called with empty list"
buildTreeFromNodeList [n] = Node n []
buildTreeFromNodeList (n:ns) = Node n [buildTreeFromNodeList ns]

-- | A node starts with a semicolon and is followed by properties.
nodeParser :: Parser SgfNode
nodeParser = do
    _ <- char ';'
    props <- many propertyParser
    -- fromListWith is used to robustly handle malformed SGF where a property
    -- identifier might appear more than once in a node. The values are
    -- concatenated.
    return $ fromListWith (++) props

-- | A property consists of an identifier and one or more values.
-- | e.g. B[aa] or AB[aa][ab]
propertyParser :: Parser (Text, [Text])
propertyParser = do
    key <- propIdentParser
    vals <- many1 propValueParser
    return (key, vals)

-- | A property identifier is a sequence of one or more uppercase letters.
propIdentParser :: Parser Text
propIdentParser = pack <$> many1 upper

-- | A property value is a string enclosed in square brackets.
propValueParser :: Parser Text
propValueParser = pack <$> between (char '[') (char ']') valueContentParser

-- | Parses the content of a property value, handling escaped characters
-- | and soft line breaks.
valueContentParser :: Parser String
valueContentParser = concat <$> many valuePartParser
  where
    valuePartParser = choice
        [ try (string "\\\n") >> return ""  -- Soft line break, ignored.
        , (:[]) <$> (char '\\' >> anyChar) -- Escaped character.
        , (:[]) <$> noneOf "]"             -- Normal character.
        ]
