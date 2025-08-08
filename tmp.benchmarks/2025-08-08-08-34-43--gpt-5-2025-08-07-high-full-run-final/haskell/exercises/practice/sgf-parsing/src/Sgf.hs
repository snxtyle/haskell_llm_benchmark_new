module Sgf (parseSgf) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | Parse an SGF string into a tree of property maps.
-- Returns Nothing on any parse error or invalid structure.
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf input = do
  (tree, rest) <- parseGameTree input
  if T.null rest then Just tree else Nothing

-- GameTree = "(" Sequence { GameTree } ")"
parseGameTree :: Text -> Maybe (SgfTree, Text)
parseGameTree t0 = do
  t1 <- consume '(' t0
  (nodes, t2) <- parseSequence t1
  (children, t3) <- parseChildren t2
  t4 <- consume ')' t3
  let tree = chainSequence nodes children
  pure (tree, t4)
  where
    parseChildren :: Text -> Maybe ([SgfTree], Text)
    parseChildren t =
      case T.uncons t of
        Just ('(', _) -> do
          (child, t1) <- parseGameTree t
          (restChildren, t2) <- parseChildren t1
          pure (child : restChildren, t2)
        _ -> Just ([], t)

    chainSequence :: [SgfNode] -> [SgfTree] -> SgfTree
    chainSequence [] _ = error "parseSequence guarantees at least one node"
    chainSequence [n] kids = Node n kids
    chainSequence (n:ns) kids = Node n [chainSequence ns kids]

-- Sequence = Node { Node }
parseSequence :: Text -> Maybe ([SgfNode], Text)
parseSequence t0 = do
  (n1, t1) <- parseNode t0
  go [n1] t1
  where
    go :: [SgfNode] -> Text -> Maybe ([SgfNode], Text)
    go acc t =
      case T.uncons t of
        Just (';', _) -> do
          (n, t') <- parseNode t
          go (acc ++ [n]) t'  -- number of nodes is small in tests; acceptable
        _ -> Just (acc, t)

-- Node = ";" PropertyList
parseNode :: Text -> Maybe (SgfNode, Text)
parseNode t0 = do
  t1 <- consume ';' t0
  parsePropertyList t1

-- PropertyList = { Property }
parsePropertyList :: Text -> Maybe (SgfNode, Text)
parsePropertyList = go Map.empty
  where
    go :: SgfNode -> Text -> Maybe (SgfNode, Text)
    go m t =
      case T.uncons t of
        Just (c, _) | isUpperAscii c -> do
          ((k, vs), t') <- parseProperty t
          if Map.member k m
            then Nothing -- duplicate key in a node
            else go (Map.insert k vs m) t'
        _ -> Just (m, t)

-- Property = PropIdent PropValue+
parseProperty :: Text -> Maybe ((Text, [Text]), Text)
parseProperty t0 = do
  (key, t1) <- parsePropIdent t0
  (v1, t2) <- parsePropValue t1
  let collect acc t =
        case T.uncons t of
          Just ('[', _) -> do
            (v, t') <- parsePropValue t
            collect (v : acc) t'
          _ -> Just (reverse acc, t)
  (vals, t3) <- collect [v1] t2
  pure ((key, vals), t3)

-- PropIdent = [A-Z]+
parsePropIdent :: Text -> Maybe (Text, Text)
parsePropIdent t0 =
  let (ident, rest) = T.span isUpperAscii t0
  in if T.null ident then Nothing else Just (ident, rest)

-- PropValue = "[" <Text value> "]"  -- SGF Text rules everywhere
parsePropValue :: Text -> Maybe (Text, Text)
parsePropValue t0 = do
  t1 <- consume '[' t0
  (content, t2) <- readValueContent t1
  pure (content, t2)

-- Parse content inside a property value until an unescaped ']' is found.
-- SGF Text rules:
-- - ']' ends the value unless escaped by backslash
-- - '\' escapes the next character
-- - backslash + newline is a line continuation and both are removed
readValueContent :: Text -> Maybe (Text, Text)
readValueContent = go []
  where
    go :: [Char] -> Text -> Maybe (Text, Text)
    go acc t =
      case T.uncons t of
        Nothing -> Nothing -- unterminated value
        Just (c, rest)
          | c == ']' -> Just (T.pack (reverse acc), rest)
          | c == '\\' ->
              case T.uncons rest of
                Nothing -> Nothing
                Just (d, rest2)
                  | d == '\n' -> go acc rest2       -- line continuation
                  | otherwise -> go (d : acc) rest2 -- escaped char included
          | otherwise -> go (c : acc) rest

-- Helpers

consume :: Char -> Text -> Maybe Text
consume ch t = do
  (c, rest) <- T.uncons t
  if c == ch then Just rest else Nothing

isUpperAscii :: Char -> Bool
isUpperAscii c = c >= 'A' && c <= 'Z'
