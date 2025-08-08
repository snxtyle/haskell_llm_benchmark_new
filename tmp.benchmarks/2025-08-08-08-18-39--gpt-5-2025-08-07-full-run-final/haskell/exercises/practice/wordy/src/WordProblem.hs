module WordProblem (answer) where

import Data.List (stripPrefix)
import Text.Read (readMaybe)

answer :: String -> Maybe Integer
answer problem = do
  body <- stripQuestionMark problem >>= stripWhatIs
  let toks = words body
  case toks of
    (t:rest) -> do
      n0 <- readInteger t
      evalOps rest n0
    _ -> Nothing

stripQuestionMark :: String -> Maybe String
stripQuestionMark s =
  if not (null s) && last s == '?'
     then Just (init s)
     else Nothing

stripWhatIs :: String -> Maybe String
stripWhatIs s = stripPrefix "What is " s

readInteger :: String -> Maybe Integer
readInteger = readMaybe

evalOps :: [String] -> Integer -> Maybe Integer
evalOps [] acc = Just acc
evalOps ("plus":t:rest) acc = do
  n <- readInteger t
  evalOps rest (acc + n)
evalOps ("minus":t:rest) acc = do
  n <- readInteger t
  evalOps rest (acc - n)
evalOps ("multiplied":"by":t:rest) acc = do
  n <- readInteger t
  evalOps rest (acc * n)
evalOps ("divided":"by":t:rest) acc = do
  n <- readInteger t
  if n == 0 then Nothing else evalOps rest (acc `div` n)
evalOps _ _ = Nothing
