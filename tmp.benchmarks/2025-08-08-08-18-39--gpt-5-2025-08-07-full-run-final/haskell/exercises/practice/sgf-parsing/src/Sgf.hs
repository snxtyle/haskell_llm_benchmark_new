module Sgf (parseSgf) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec (Parsec, anyChar, char, choice, eof, many, many1, noneOf, parse, string, try, unexpected, between)

-- Public API
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf sgf =
  case parse sgfParser "sgf" (T.unpack sgf) of
    Left _   -> Nothing
    Right tr -> Just tr

-- Parser for a full SGF document: a single game tree
sgfParser :: Parsec String () (Tree (Map Text [Text]))
sgfParser = do
  t <- gameTree
  eof
  pure t

-- gametree  = "(" sequence gametree* ")"
gameTree :: Parsec String () (Tree (Map Text [Text]))
gameTree = do
  _      <- char '('
  nodes  <- sequenceNodes
  kids   <- many gameTree
  _      <- char ')'
  pure (chainNodes nodes kids)

-- sequence  = node+
sequenceNodes :: Parsec String () [Map Text [Text]]
sequenceNodes = many1 node

-- node      = ";" property*
node :: Parsec String () (Map Text [Text])
node = do
  _      <- char ';'
  props  <- many propertyP
  let m = Map.fromList props
  if Map.size m /= length props
    then unexpected "duplicate property key in node"
    else pure m

-- property  = prop_ident prop_value+
propertyP :: Parsec String () (Text, [Text])
propertyP = do
  k  <- propIdent
  vs <- many1 propValue
  pure (k, vs)

-- prop_ident = UPPER+
propIdent :: Parsec String () Text
propIdent = T.pack <$> many1 upperAscii
  where
    upperAscii = choice (map char ['A'..'Z'])

-- prop_value = "[" value_contents "]"
propValue :: Parsec String () Text
propValue = between (char '[') (char ']') (T.pack . concat <$> many valuePiece)

-- value contents:
-- - "\<newline>" is a line continuation and is removed
-- - "\" escapes the next character (e.g. "\]" -> "]", "\\" -> "\")
-- - any other character except ']' is literal
valuePiece :: Parsec String () String
valuePiece = choice
  [ try (string "\\\n" >> pure "")
  , do _ <- char '\\'
       c <- anyChar
       pure [c]
  , many1 (noneOf "]\\")
  ]

-- Build a single-branch chain of nodes with subtrees attached to the last node.
chainNodes :: [a] -> [Tree a] -> Tree a
chainNodes [] _       = error "chainNodes: empty sequence (parser should prevent this)"
chainNodes [x] kids   = Node x kids
chainNodes (x:xs) kids = Node x [chainNodes xs kids]
