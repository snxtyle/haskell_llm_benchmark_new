module Sgf (parseSgf) where

import           Control.Applicative        ((<|>), many)
import           Data.Char                  (isAlpha, isUpper)
import           Data.Foldable              (foldlM)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Tree                  (Tree (..))
import           Text.Parsec                (ParseError, Parsec, anyChar, between, char, choice, eof, many1, manyTill, noneOf, option, optionMaybe, parse, try, (<?>))
import qualified Text.Parsec                as P

-- public API
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf =
  case parse pCollection "sgf" (T.unpack sgf) of
    Right t -> Just t
    Left _  -> Nothing

-- Grammar (subset sufficient for exercism tests):
-- Collection := GameTree
-- GameTree  := '(' Sequence { GameTree } ')'
-- Sequence  := Node { Node }
-- Node      := ';' { Property }
-- Property  := PropIdent PropValue+
-- PropIdent := UPPERCASE+
-- PropValue := '[' ValueContent ']'
-- ValueContent: SGF "text" rules: inside brackets, ']' ends value unless escaped with '\'.
-- Backslash escapes next char; newlines following '\' are treated as a single whitespace.
-- We will normalize: convert any newline characters to '\n' and keep them, and treat escaped
-- sequences by removing the escape and including the char. Also, unescape escaped newlines.

type Parser = Parsec String ()

pCollection :: Parser (Tree (Map Text [Text]))
pCollection = do
  t <- pGameTree
  eof
  return t

pGameTree :: Parser (Tree (Map Text [Text]))
pGameTree = between (char '(') (char ')') $ do
  (rootNode:restNodes) <- pSequence
  let rootTree = Node rootNode []
      appendChild (Node v cs) n = Node v (cs ++ [Node n []])
      seqTree = foldl appendChild rootTree restNodes
  children <- many pGameTree
  let Node v cs = seqTree
  return (Node v (cs ++ children))

pSequence :: Parser [Map Text [Text]]
pSequence = many1 pNode

pNode :: Parser (Map Text [Text])
pNode = do
  _ <- char ';'
  props <- many pProperty
  -- ensure unique keys
  foldlM insertProp M.empty props
  where
    insertProp acc (k, vs) =
      if M.member k acc
        then fail "duplicate property key in node"
        else return (M.insert k vs acc)

pProperty :: Parser (Text, [Text])
pProperty = do
  ident <- pPropIdent
  values <- many1 pPropValue
  return (ident, values)

pPropIdent :: Parser Text
pPropIdent = do
  cs <- many1 (P.satisfy isUpper) <?> "property identifier"
  return (T.pack cs)

pPropValue :: Parser Text
pPropValue = between (char '[') (char ']') pValueContent

pValueContent :: Parser Text
pValueContent = T.pack <$> many (pEscapedChar <|> pNormalChar)
  where
    -- escaped: backslash escapes next char including ']' or '\' or newline
    pEscapedChar :: Parser Char
    pEscapedChar = do
      _ <- char '\\'
      -- If the next is newline, consume any following newline and return a space
      choice
        [ newlineWhitespace
        , anyChar -- return the char literally
        ]

    newlineWhitespace :: Parser Char
    newlineWhitespace = do
      -- In SGF, a backslash followed by a newline (and an optional carriage return)
      -- is treated as a line continuation and should be converted to a space.
      _ <- P.optional (char '\r')
      _ <- char '\n'
      -- swallow any following spaces (not strictly necessary for our constraints)
      _ <- many (char ' ' <|> char '\t' <|> char '\r' <|> char '\n')
      return ' '

    pNormalChar :: Parser Char
    pNormalChar = noneOf "]"

-- utilities
many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p
