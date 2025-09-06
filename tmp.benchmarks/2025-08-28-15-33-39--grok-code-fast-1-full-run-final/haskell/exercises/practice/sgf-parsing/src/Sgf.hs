module Sgf (parseSgf) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.Text (Parser)

parseSgf :: Text -> Maybe (Tree (Map Text [Text]))
parseSgf input = case parse (sgfParser <* eof) "" input of
    Right tree -> Just tree
    Left _ -> Nothing

sgfParser :: Parser (Tree (Map Text [Text]))
sgfParser = gametree

gametree :: Parser (Tree (Map Text [Text]))
gametree = between (char '(') (char ')') sequenceTree

sequenceTree :: Parser (Tree (Map Text [Text]))
sequenceTree = do
    nodes <- many1 node
    subs <- many gametree
    return $ buildTree nodes subs

buildTree :: [Map Text [Text]] -> [Tree (Map Text [Text])] -> Tree (Map Text [Text])
buildTree [] _ = error "Empty node sequence"
buildTree [n] subs = Node n subs
buildTree (n:ns) subs = Node n [buildTree ns subs]

node :: Parser (Map Text [Text])
node = char ';' >> (Map.fromList <$> many prop)

prop :: Parser (Text, [Text])
prop = do
    key <- many1 upper
    vals <- many1 propValue
    return (T.pack key, vals)

propValue :: Parser Text
propValue = T.pack <$> between (char '[') (char ']') (many (try escaped <|> satisfy (/= ']')))
  where
    escaped = char '\\' >> anyChar
