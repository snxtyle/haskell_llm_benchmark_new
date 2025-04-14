{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.Map.Strict as M
import Data.List (foldl')
import Data.Bifunctor (first)
import Control.Monad (foldM)
import Data.Maybe (fromMaybe)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

type Stack = [Int]
type Dict = M.Map Text [Token]

data ForthState = ForthState
  { stack :: Stack
  , dict  :: Dict
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] M.empty

data Token
  = Number Int
  | Word Text
  | DefStart
  | DefEnd
  deriving (Show, Eq)

-- Tokenize input text into tokens
tokenize :: Text -> [Token]
tokenize = go . T.words . T.toLower
  where
    go [] = []
    go (t:ts)
      | t == ":"  = DefStart : go ts
      | t == ";"  = DefEnd   : go ts
      | T.all C.isDigit t || (T.head t == '-' && T.length t > 1 && T.all C.isDigit (T.tail t))
                  = Number (read (T.unpack t)) : go ts
      | otherwise = Word t : go ts

-- Evaluate a list of tokens
evalTokens :: [Token] -> ForthState -> Either ForthError ForthState
evalTokens [] st = Right st
evalTokens (DefStart : Word w : rest) st
  | T.all C.isDigit w = Left InvalidWord
  | otherwise =
      let (defBody, afterDef) = break (== DefEnd) rest
      in if null afterDef
         then Left InvalidWord
         else
           let newDict = M.insert w defBody (dict st)
           in evalTokens (tail afterDef) (st { dict = newDict })
evalTokens (DefStart : _) _ = Left InvalidWord
evalTokens (Number n : ts) st = evalTokens ts (st { stack = stack st ++ [n] })
evalTokens (Word w : ts) st =
  case builtin w of
    Just op -> op st >>= evalTokens ts
    Nothing ->
      case M.lookup w (dict st) of
        Just body -> evalTokens (body ++ ts) st
        Nothing   -> Left (UnknownWord w)
evalTokens (DefEnd : _) _ = Left InvalidWord

-- Built-in operations
builtin :: Text -> Maybe (ForthState -> Either ForthError ForthState)
builtin "+" = Just $ binOp (+)
builtin "-" = Just $ binOp (-)
builtin "*" = Just $ binOp (*)
builtin "/" = Just $ \st -> case pop2 (stack st) of
  Just (a, 0, rest) -> Left DivisionByZero
  Just (a, b, rest) -> Right st { stack = rest ++ [a `div` b] }
  Nothing           -> Left StackUnderflow
builtin "dup" = Just $ \st -> case stack st of
  [] -> Left StackUnderflow
  xs -> Right st { stack = xs ++ [last xs] }
builtin "drop" = Just $ \st -> case stack st of
  [] -> Left StackUnderflow
  xs -> Right st { stack = init xs }
builtin "swap" = Just $ \st -> case stack st of
  xs@(_:_:_) ->
    let (initStack, [a, b]) = splitAt (length xs - 2) xs
    in Right st { stack = initStack ++ [b, a] }
  _ -> Left StackUnderflow
builtin "over" = Just $ \st -> case stack st of
  xs@(_:_:_) ->
    let (initStack, [a, b]) = splitAt (length xs - 2) xs
    in Right st { stack = initStack ++ [a, b, a] }
  _ -> Left StackUnderflow
builtin _ = Nothing

binOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binOp f st = case pop2 (stack st) of
  Just (a, b, rest) -> Right st { stack = rest ++ [a `f` b] }
  Nothing           -> Left StackUnderflow

pop2 :: [Int] -> Maybe (Int, Int, [Int])
pop2 xs
  | length xs < 2 = Nothing
  | otherwise =
      let (rest, [a, b]) = splitAt (length xs - 2) xs
      in Just (a, b, rest)

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text st = evalTokens (tokenize text) st

toList :: ForthState -> [Int]
toList = stack
