module Sgf (parseSgf) where

import Data.Tree (Tree(Node))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Text.Parsec (Parsec, parse, (<|>), try, eof)
import Text.Parsec.Char (char, upper, noneOf) -- Removed unused 'string' and 'letter'
import Text.Parsec.Combinator (many1, many) -- many1 is equivalent to 'some'

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | Main parsing function. Converts Text to String for Parsec.
parseSgf :: Text -> Maybe SgfTree
parseSgf sgfInput =
  case parse pSgfCollection "(sgf)" (T.unpack sgfInput) of
    Left _err -> Nothing -- Optionally, could log _err for debugging
    Right tree -> Just tree

-- | Parser for the entire SGF content, expecting one game tree followed by EOF.
pSgfCollection :: Parsec String () SgfTree
pSgfCollection = pGameTree <* eof

-- | Parser for a game tree: ( Node+ Variation* )
-- A game tree is a sequence of one or more nodes, optionally followed by variations (sub-trees).
pGameTree :: Parsec String () SgfTree
pGameTree = do
  _ <- char '('
  nodes <- many1 pNode -- Parses one or more ';PropList'
  variations <- many pGameTree -- Parses zero or more variations '(GameTree)'
  _ <- char ')'
  return (buildTree nodes variations)

-- | Helper function to construct an SgfTree from a list of SgfNodes and a list of variations.
-- The nodes form a path, and the variations are children of the last node in that path.
-- e.g., [N1, N2, N3] with vars [V1, V2] becomes N1 -- N2 -- N3 -- [V1, V2]
buildTree :: [SgfNode] -> [SgfTree] -> SgfTree
buildTree [] _ = error "buildTree: Called with empty node list. This should be prevented by many1 pNode."
buildTree [n] vars = Node n vars
buildTree (n:ns) vars = Node n [buildTree ns vars]

-- | Parser for a single SGF node: starts with ';' followed by properties.
pNode :: Parsec String () SgfNode
pNode = do
  _ <- char ';'
  properties <- many1 pProperty
  -- SGF spec says: "Each key can only occur once in a property list."
  -- If duplicate keys were allowed and meant to be merged, Map.fromListWith would be needed.
  -- Given the spec, Map.fromList is appropriate.
  return (Map.fromList properties)

-- | Parser for a property: PropIdent PropValue+
-- Returns a tuple of (PropertyIdentifier, ListOfPropertyValues).
pProperty :: Parsec String () (Text, [Text])
pProperty = do
  ident <- pPropIdent
  values <- many1 pPropValue
  return (T.pack ident, map T.pack values)

-- | Parser for a property identifier: one or more uppercase letters.
pPropIdent :: Parsec String () String -- Returns String, converted to Text in pProperty
pPropIdent = many1 upper

-- | Parser for a property value: text enclosed in square brackets.
pPropValue :: Parsec String () String -- Returns String, converted to Text in pProperty
pPropValue = do
  _ <- char '['
  -- According to SGF spec for TEXT type:
  -- - Soft line breaks are removed (not handled here as per problem statement simplification)
  -- - '\' and ']' must be escaped.
  val <- many pValueChar
  _ <- char ']'
  return val

-- | Parser for a single character within a property value.
-- Handles escaped characters: '\\' followed by ']' or '\'.
-- Otherwise, parses any character that is not ']' or '\'.
pValueChar :: Parsec String () Char
pValueChar =
      (char '\\' *> (char ']' <|> char '\\')) -- Escaped: \] or \\
  <|> noneOf "\\]"                             -- Any other character except ] or \
