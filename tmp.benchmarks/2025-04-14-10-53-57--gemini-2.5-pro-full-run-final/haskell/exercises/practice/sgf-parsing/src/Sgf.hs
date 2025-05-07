{-# LANGUAGE OverloadedStrings #-}
module Sgf (parseSgf, SgfTree, SgfNode) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec (parse, (<|>), (<?>), choice, many, many1, between, char, eof, upper, noneOf, anyChar, ParseError)
import Text.Parsec.Text (Parser)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- Parser for property value content (handles escapes)
-- SGF escapes ']' and '\' by prefixing them with '\'.
propValueContent :: Parser Text
propValueContent = T.pack <$> many valueChar
  where
    valueChar = choice
      [ char '\\' *> anyChar -- Consume escaped character (any character after '\')
      , noneOf "\\]"         -- Consume regular character (anything but \ or ])
      ] <?> "property value content"

-- Parser for a single property value enclosed in []
propValue :: Parser Text
propValue = between (char '[') (char ']') propValueContent <?> "property value"

-- Parser for property identifier (uppercase letters)
propIdent :: Parser Text
propIdent = T.pack <$> many1 upper <?> "property identifier"

-- Parser for a single property (Identifier[Value][Value]...)
property :: Parser (Text, [Text])
property = do
  ident <- propIdent
  vals <- many1 propValue
  return (ident, vals)
  <?> "property"

-- Parser for a node (;Prop1 Prop2...)
-- Properties are aggregated into a Map. If a key appears multiple times,
-- the values are concatenated in the order they appear.
sgfNode :: Parser SgfNode
sgfNode = do
  _ <- char ';'
  props <- many property
  -- fromListWith combines values when keys are duplicated.
  -- (++) ensures new values are appended to existing ones.
  return $ Map.fromListWith (++) props
  <?> "node"

-- Parser for a tree or subtree ((;Node...)(Subtree...))
sgfTree :: Parser SgfTree
sgfTree = between (char '(') (char ')') parseSequence <?> "tree"
  where
    -- A sequence consists of one or more nodes followed by zero or more trees.
    parseSequence :: Parser SgfTree
    parseSequence = do
      nodes <- many1 sgfNode
      trees <- many sgfTree
      return $ buildTree (nodes, trees)

    -- Helper to construct the tree structure from parsed nodes and subtrees.
    -- The first node is the root. Subsequent nodes form a linear sequence
    -- as the first child branch. Parsed subtrees become subsequent children
    -- of the root.
    buildTree :: ([SgfNode], [SgfTree]) -> SgfTree
    buildTree ([], _) = error "Impossible state: empty node sequence from many1 sgfNode" -- Should not happen due to many1
    buildTree (rootNode:chainNodes, subTrees) = Node rootNode (buildChain chainNodes ++ subTrees)

    -- Helper to build the linear chain of nodes.
    buildChain :: [SgfNode] -> [SgfTree]
    buildChain [] = []
    buildChain (n:ns) = [Node n (buildChain ns)] -- Each node becomes the root of a subtree whose forest is the rest of the chain.

-- Main parsing function
-- It attempts to parse the entire input Text as a single SgfTree.
parseSgf :: Text -> Maybe SgfTree
parseSgf input = case parse (sgfTree <* eof) "(sgf input)" input of
    Left _err -> Nothing -- Parsing failed
    Right tree -> Just tree -- Parsing succeeded
