module Sgf (parseSgf) where

import Data.Map (Map, fromList)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Tree (Tree(..))
import Text.Parsec
import Text.Parsec.String (Parser)

type SgfNode = Map Text [Text]

parseSgf :: Text -> Maybe (Tree SgfNode)
parseSgf s = case parse (sgfTree <* eof) "" (T.unpack s) of
               Left _ -> Nothing
               Right tree -> Just tree

sgfTree :: Parser (Tree SgfNode)
sgfTree = do
  _ <- char '('
  nodes <- many1 nodeParser
  variations <- many sgfTree
  _ <- char ')'
  return $ buildTree nodes variations

nodeParser :: Parser SgfNode
nodeParser = do
  _ <- char ';'
  props <- many property
  return $ fromList props

property :: Parser (Text, [Text])
property = do
  key <- many1 upper
  vals <- many1 (between (char '[') (char ']') textValue)
  return (pack key, map pack vals)

textValue :: Parser String
textValue = do
  parts <- many (escapedChar <|> normalChar)
  return (concat parts)
  where
    normalChar = do
      c <- noneOf "]"
      case c of
        '\t' -> return " "
        _    -> return [c]
    escapedChar = do
      _ <- char '\\'
      c <- oneOf "\\]tn "
      case c of
        't' -> return " "
        'n' -> return ""
        ' ' -> return ""
        '\\' -> return "\\"
        ']' -> return "]"

buildTree :: [SgfNode] -> [Tree SgfNode] -> Tree SgfNode
buildTree nodes vs = case nodes of
  [x] -> Node x vs
  (x:xs) -> Node x [buildTree xs vs]
