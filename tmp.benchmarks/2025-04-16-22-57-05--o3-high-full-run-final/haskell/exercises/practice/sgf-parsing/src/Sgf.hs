{-# LANGUAGE OverloadedStrings #-}

module Sgf (parseSgf) where

import           Control.Applicative        ((<|>), many)
import           Data.List                  (nub)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tree                  (Tree (..))
import           Text.Parsec                (Parsec, anyChar, char, eof,
                                             many1, noneOf, oneOf,
                                             optionMaybe, parse, parserFail,
                                             try, upper, (<|>))

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | Entry point: parse an SGF string into a tree of nodes.
--   Returns Nothing on any parsing error.
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgfText =
  case parse (pGameTree <* eof) "" (T.unpack sgfText) of
    Left _      -> Nothing
    Right sgf   -> Just sgf

-- ---------------------------------------------------------------------
-- Parsers
-- ---------------------------------------------------------------------

type Parser = Parsec String ()

-- game_tree = "(" sequence { game_tree } ")"
pGameTree :: Parser SgfTree
pGameTree = do
  _        <- char '('
  seqNodes <- pSequence
  subTrees <- many pGameTree
  _        <- char ')'
  return $ buildTree seqNodes subTrees

-- sequence = node { node }
pSequence :: Parser [SgfNode]
pSequence = many1 pNode

-- node = ";" { property }
pNode :: Parser SgfNode
pNode = do
  _      <- char ';'
  props  <- many pProperty
  buildNode props

-- property = prop_ident 1*(prop_value)
pProperty :: Parser (Text, [Text])
pProperty = do
  ident  <- pPropIdent
  values <- many1 pPropValue
  return (ident, values)

-- prop_ident = one or more uppercase letters
pPropIdent :: Parser Text
pPropIdent = T.pack <$> many1 upper

-- prop_value = "[" value "]"
pPropValue :: Parser Text
pPropValue = do
  _       <- char '['
  content <- many pValueChar
  _       <- char ']'
  return $ T.pack content

-- Content inside a prop_value
pValueChar :: Parser Char
pValueChar = try escapedChar <|> noneOf "]"

-- Handle escapes inside property values
escapedChar :: Parser Char
escapedChar = do
  _ <- char '\\'
  escapedNewline <|> oneOf "[]\\"

-- Backslash followed by newline (or CR/LF) – treated as a continuation.
-- We remove it completely, but for technical reasons return a newline.
escapedNewline :: Parser Char
escapedNewline = do
  _ <- oneOf "\n\r"
  _ <- optionMaybe (char '\n')  -- swallow the second char in CRLF
  return '\n'

-- ---------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------

-- Build a node while rejecting duplicate property identifiers.
buildNode :: [(Text, [Text])] -> Parser SgfNode
buildNode props =
  if hasDuplicates (map fst props)
     then parserFail "Duplicate property identifiers within node"
     else return (Map.fromList props)
  where
    hasDuplicates xs = length xs /= length (nub xs)

-- Construct the final tree from the parsed sequence and its variations.
buildTree :: [SgfNode] -> [SgfTree] -> SgfTree
buildTree [] _ = error "Sequence must contain at least one node."
buildTree (n:ns) subs = Node n (buildChildren ns subs)
  where
    buildChildren :: [SgfNode] -> [SgfTree] -> [SgfTree]
    buildChildren []     ch = ch
    buildChildren (x:xs) ch = [Node x (buildChildren xs ch)]
