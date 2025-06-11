{-# LANGUAGE OverloadedStrings #-}
module Sgf (parseSgf) where

import           Control.Monad          (foldM, when)
import           Data.Maybe             (catMaybes)
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict as Map
import           Data.Text              (Text)
import qualified Data.Text        as T
import           Data.Tree              (Tree (..))
import           Text.Parsec
import           Text.Parsec.Text       (Parser)

-- | Public API -------------------------------------------------------------

-- | Parse an SGF string into a tree of nodes.
--   Returns Nothing if the input is not a valid, single SGF game tree.
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf input =
  case parse (spaces *> gameTree <* eof) "sgf" input of
    Left _err -> Nothing
    Right t   -> Just t

-- | Internal ----------------------------------------------------------------

-- A node is a property list, each key can only occur once.
type SgfNode = Map Text [Text]

-- | Parse a complete game tree: '(' Sequence ')'
gameTree :: Parser (Tree SgfNode)
gameTree = between (char '(') (char ')') sequenceP

-- | Parse a sequence: one or more nodes, optionally followed by variations.
sequenceP :: Parser (Tree SgfNode)
sequenceP = do
  nodes      <- some sgfNode
  variations <- many gameTree
  return (buildLinear nodes variations)

-- | Build a linear chain of nodes.
--   The variations are attached to the *last* node in the chain.
buildLinear :: [SgfNode] -> [Tree SgfNode] -> Tree SgfNode
buildLinear [] _      = error "buildLinear called with empty list"
buildLinear [n] vars  = Node n vars
buildLinear (n:ns) vars = Node n [buildLinear ns vars]

-- | Parse a node: ';' followed by one or more properties
sgfNode :: Parser SgfNode
sgfNode = do
  _ <- char ';'
  props <- many property
  when (null props) $
    parserFail "Node must contain at least one property"
  toMap props

-- | Convert property list to Map, failing on duplicate keys.
toMap :: [(Text, [Text])] -> Parser SgfNode
toMap = foldM go Map.empty
  where
    go m (k, vs)
      | Map.member k m = parserFail "Duplicate property key in node"
      | otherwise      = return (Map.insert k vs m)

-- | Parse a single property: Key Value+
property :: Parser (Text, [Text])
property = do
  key <- propertyIdent
  values <- some propertyValue
  return (key, values)

-- | Property identifier: one or more uppercase letters
propertyIdent :: Parser Text
propertyIdent = T.pack <$> some (oneOf ['A'..'Z'])

-- | Property value: '[' ... ']'
propertyValue :: Parser Text
propertyValue =
  between (char '[') (char ']') (T.pack . catMaybes <$> many valueChar)

-- | Characters inside a property value, handling escapes and newlines.
valueChar :: Parser (Maybe Char)
valueChar = escaped <|> newlineAsSpace <|> normalChar
  where
    -- Backslash escapes the next character (including ']')
    escaped = do
      _ <- char '\\'
      next <- anyChar
      case next of
        '\n'         -> return Nothing           -- line continuation
        '\r'         -> optional (char '\n') >> return Nothing
        _            -> return (Just next)
    -- Bare newlines (or CRLF) become a single space
    newlineAsSpace = do
      c <- oneOf "\n\r"
      when (c == '\r') (optional (char '\n'))
      return (Just ' ')
    -- Any character except ']' (terminator)
    normalChar = do
      c <- noneOf "]\r\n"
      return (Just c)
