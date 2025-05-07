{-# LANGUAGE OverloadedStrings #-}
module Sgf (parseSgf) where

import           Control.Monad              (void)
import           Data.Foldable              (foldlM)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tree                  (Tree (..))
import           Text.Parsec                (ParseError, Parsec, anyChar,
                                             between, char, eof, lookAhead,
                                             many1, manyTill, noneOf, option,
                                             parse, parserFail, try, (<|>))
import           Text.Parsec.Text           (Parser)

-- | Parse an SGF string into a rose tree of nodes.
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf input =
  case parse (sgfTreeParser <* eof) "sgf" input of
    Left _  -> Nothing
    Right r -> Just r

----------------------------------------------------------------------
-- Internal parsers
----------------------------------------------------------------------

-- Top‑level tree parser: one SGF collection that contains exactly one game.
sgfTreeParser :: Parser (Tree (Map Text [Text]))
sgfTreeParser = between (char '(') (char ')') $ do
  seqNodes <- many1 nodeParser               -- at least one node
  branches <- many sgfTreeParser             -- 0 or more variations
  return (chainSequence seqNodes branches)

-- | Build a tree from the sequential nodes followed by the branch list.
--   The first node is the root, subsequent nodes are chained as the single
--   child of their predecessor.  After the sequence ends, we attach the
--   variation branches to the final node in the chain.
chainSequence :: [Map Text [Text]] -> [Tree (Map Text [Text])] -> Tree (Map Text [Text])
chainSequence []     _       = error "chainSequence: empty list (impossible)"
chainSequence (n:ns) subTrees = go n ns
  where
    go x []     = Node x subTrees
    go x (y:ys) = Node x [go y ys]

----------------------------------------------------------------------
-- Node parser
----------------------------------------------------------------------

nodeParser :: Parser (Map Text [Text])
nodeParser = do
  void (char ';')
  props <- many propertyParser               -- nodes may have zero properties
  buildNode props

-- | Helper that fails if a property key occurs more than once.
buildNode :: [(Text, [Text])] -> Parser (Map Text [Text])
buildNode = foldlM insertUnique M.empty
  where
    insertUnique acc (k, vs) =
      if M.member k acc
         then parserFail "duplicate property identifier in node"
         else return (M.insert k vs acc)

----------------------------------------------------------------------
-- Property parser
----------------------------------------------------------------------

propertyParser :: Parser (Text, [Text])
propertyParser = do
  ident  <- identParser
  values <- many1 propertyValue
  return (ident, values)

-- | Property identifier: non‑empty sequence of uppercase letters.
identParser :: Parser Text
identParser = fmap T.pack (many1 upperChar)
  where
    upperChar = oneOfRange 'A' 'Z'
    oneOfRange a z = do
      c <- anyChar
      if c >= a && c <= z
         then return c
         else parserFail "property identifiers must be uppercase letters"

----------------------------------------------------------------------
-- Property value parser
----------------------------------------------------------------------

propertyValue :: Parser Text
propertyValue = between (char '[') (char ']') $ do
  chars <- manyTill valueChar (lookAhead (char ']'))
  return . T.pack $ filter (/= '\0') chars
  where
    -- Parse a single character inside a property value, handling escapes.
    valueChar :: Parser Char
    valueChar = escapedChar <|> noneOf "]"

    escapedChar :: Parser Char
    escapedChar = try $ do
      void (char '\\')
      c <- anyChar
      case c of
        '\n' -> return '\0'   -- remove backslash‑newline
        '\r' -> return '\0'
        _    -> return c
