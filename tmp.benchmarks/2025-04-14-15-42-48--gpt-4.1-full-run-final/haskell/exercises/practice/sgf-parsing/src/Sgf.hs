module Sgf (parseSgf) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Data.Char (isUpper)
import Data.Bifunctor (first)

-- | A tree of nodes.
type SgfTree = Tree SgfNode

-- | A node is a property list, each key can only occur once.
-- Keys may have multiple values associated with them.
type SgfNode = Map Text [Text]

-- | Main entry point
parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf t =
  case parseSgfTree (T.unpack t) of
    Just (tree, rest) | all (`elem` " \n\t\r") rest -> Just tree
    _ -> Nothing

-- Parser type: a function from String to Maybe (a, String)
type Parser a = String -> Maybe (a, String)

-- Basic parser combinators

satisfy :: (Char -> Bool) -> Parser Char
satisfy p (c:cs) | p c = Just (c, cs)
satisfy _ _ = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

-- Redefine many and many1 to avoid ambiguity with Control.Applicative
many :: Parser a -> Parser [a]
many p = go
  where
    go s = case p s of
      Just (x, s') -> case go s' of
        Just (xs, s'') -> Just (x:xs, s'')
        Nothing -> Just ([x], s')
      Nothing -> Just ([], s)

many1 :: Parser a -> Parser [a]
many1 p = \s -> case p s of
  Just (x, s') -> case many p s' of
    Just (xs, s'') -> Just (x:xs, s'')
    Nothing -> Just ([x], s')
  Nothing -> Nothing

optional :: Parser a -> Parser (Maybe a)
optional p = (\x -> Just x) <$> p <|> pure Nothing

choice :: [Parser a] -> Parser a
choice = foldr (<|>) empty
  where empty _ = Nothing

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close p = open *> p <* close

skipSpaces :: Parser ()
skipSpaces = many (satisfy (`elem` " \n\t\r")) *> pure ()

-- SGF-specific parsers

parseSgfTreeP :: Parser SgfTree
parseSgfTreeP = skipSpaces *> between (char '(') (char ')') parseTreeSeq

parseTreeSeq :: Parser SgfTree
parseTreeSeq = do
  nodes <- many1 parseNode
  children <- many parseVariation
  let tree = buildTree nodes children
  pure tree

parseVariation :: Parser SgfTree
parseVariation = parseSgfTreeP

parseNode :: Parser SgfNode
parseNode = skipSpaces *> char ';' *> parsePropertyList

parsePropertyList :: Parser SgfNode
parsePropertyList = do
  props <- many parseProperty
  pure $ Map.fromListWith (++) props

parseProperty :: Parser (Text, [Text])
parseProperty = do
  key <- parsePropIdent
  vals <- many1 parsePropValue
  pure (T.pack key, vals)

parsePropIdent :: Parser String
parsePropIdent = many1 (satisfy isUpper)

parsePropValue :: Parser Text
parsePropValue = do
  _ <- char '['
  val <- many parsePropValueChar
  _ <- char ']'
  pure (T.pack val)

parsePropValueChar :: Parser Char
parsePropValueChar = (char '\\' *> parseEscapedChar) <|> satisfy (\c -> c /= ']' && c /= '\\')

parseEscapedChar :: Parser Char
parseEscapedChar = satisfy (const True)

-- Build a tree from a list of nodes and children
buildTree :: [SgfNode] -> [SgfTree] -> SgfTree
buildTree (n:ns) children = Node n (buildForest ns children)
buildTree [] _ = error "No node to build tree from"

buildForest :: [SgfNode] -> [SgfTree] -> [SgfTree]
buildForest [] children = children
buildForest (n:ns) children = [Node n (buildForest ns children)]

-- Run a parser
runParser :: Parser a -> String -> Maybe (a, String)
runParser = id

-- Top-level parse function for a tree
parseSgfTree :: String -> Maybe (SgfTree, String)
parseSgfTree = runParser parseSgfTreeP

-- Applicative/Alternative combinators (local, to avoid ambiguity)
infixl 4 <*>, <$>, *>, <*

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
pf <*> pa = \s -> case pf s of
  Just (f, s') -> case pa s' of
    Just (a, s'') -> Just (f a, s'')
    Nothing -> Nothing
  Nothing -> Nothing

(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> pa = \s -> case pa s of
  Just (a, s') -> Just (f a, s')
  Nothing -> Nothing

(*>) :: Parser a -> Parser b -> Parser b
pa *> pb = (\_ b -> b) <$> pa <*> pb

(<*) :: Parser a -> Parser b -> Parser a
pa <* pb = (\a _ -> a) <$> pa <*> pb

pure :: a -> Parser a
pure a = \s -> Just (a, s)

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = \s -> case p1 s of
  Nothing -> p2 s
  res -> res
