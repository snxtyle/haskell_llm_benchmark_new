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
import Data.Maybe (fromMaybe)
import Control.Monad (foldM)
import Data.Bifunctor (first)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- ForthState holds the stack and the dictionary of defined words
data ForthState = ForthState
  { stack :: [Int]
  , dict  :: M.Map Text [Text]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] M.empty

-- Convert the stack to list with top of stack as last element
toList :: ForthState -> [Int]
toList = stack

-- Evaluate a Text input on a given ForthState, returning either an error or new state
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input state = evalTokens (tokenize input) state

-- Tokenize input text into words, case-insensitive (lowercase)
tokenize :: Text -> [Text]
tokenize = map T.toLower . T.words

-- Evaluate a list of tokens on a given state
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens (":":rest) state = defineWord rest state
evalTokens (t:ts) state = do
  state' <- evalWord t state
  evalTokens ts state'

-- Define a new word: syntax ": word-name definition ;"
defineWord :: [Text] -> ForthState -> Either ForthError ForthState
defineWord [] _ = Left InvalidWord
defineWord (name:rest) state
  | T.null name = Left InvalidWord
  | T.any C.isDigit (T.take 1 name) = Left InvalidWord -- word name cannot start with digit
  | otherwise = case break (== ";") rest of
      (definition, [] ) -> Left InvalidWord -- no terminating ;
      (definition, _ : after) -> do
        -- Check definition does not contain invalid word names (numbers are allowed in definition)
        -- But we allow any tokens in definition, as they can be numbers or words
        -- Store definition in dictionary
        let newDict = M.insert name definition (dict state)
        let newState = state { dict = newDict }
        -- Continue evaluating after the definition
        evalTokens after newState

-- Evaluate a single word/token on the state
evalWord :: Text -> ForthState -> Either ForthError ForthState
evalWord w state
  | Just def <- M.lookup w (dict state) = evalTokens def state
  | Just n <- readNumber w = Right $ state { stack = stack state ++ [n] }
  | otherwise = evalBuiltin w state

-- Try to parse a number (decimal, signed)
readNumber :: Text -> Maybe Int
readNumber t =
  case T.uncons t of
    Just ('-', rest) | not (T.null rest) && T.all C.isDigit rest -> Just (negate (readInt rest))
    _ | T.all C.isDigit t -> Just (readInt t)
    _ -> Nothing
  where
    readInt = read . T.unpack

-- Evaluate built-in words
evalBuiltin :: Text -> ForthState -> Either ForthError ForthState
evalBuiltin "+" = binaryOp (+)
evalBuiltin "-" = binaryOp (-)
evalBuiltin "*" = binaryOp (*)
evalBuiltin "/" = divOp
evalBuiltin "dup" = dupOp
evalBuiltin "drop" = dropOp
evalBuiltin "swap" = swapOp
evalBuiltin "over" = overOp
evalBuiltin w _ = Left (UnknownWord w)

-- Helper to pop two values and push the result of op
binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state =
  case stack state of
    (x:y:xs) -> Right $ state { stack = xs ++ [y `op` x] }
    _ -> Left StackUnderflow

-- Division with check for zero divisor
divOp :: ForthState -> Either ForthError ForthState
divOp state =
  case stack state of
    (0:_:_) -> Left DivisionByZero
    (x:y:xs) -> Right $ state { stack = xs ++ [y `div` x] }
    _ -> Left StackUnderflow

-- DUP duplicates the top of the stack
dupOp :: ForthState -> Either ForthError ForthState
dupOp state =
  case stack state of
    [] -> Left StackUnderflow
    xs -> Right $ state { stack = xs ++ [last xs] }

-- DROP removes the top of the stack
dropOp :: ForthState -> Either ForthError ForthState
dropOp state =
  case stack state of
    [] -> Left StackUnderflow
    xs -> Right $ state { stack = init xs }

-- SWAP swaps the top two elements
swapOp :: ForthState -> Either ForthError ForthState
swapOp state =
  case stack state of
    (x:y:xs) -> Right $ state { stack = xs ++ [x,y] }
    _ -> Left StackUnderflow

-- OVER copies the second element to the top
overOp :: ForthState -> Either ForthError ForthState
overOp state =
  case stack state of
    (x:y:xs) -> Right $ state { stack = xs ++ [y,x,y] }
    _ -> Left StackUnderflow
