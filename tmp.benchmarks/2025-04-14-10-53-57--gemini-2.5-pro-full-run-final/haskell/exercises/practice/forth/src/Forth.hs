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
import qualified Data.Text.Read as TR
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isDigit)
import Data.List (foldl')
import Control.Monad (foldM)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- ForthState holds the stack and user-defined word definitions.
-- The stack is represented as a list where the head is the top.
-- Definitions map lowercase word names to their sequence of operations (as Text).
data ForthState = ForthState
  { stack :: [Int]
  , definitions :: Map Text [Text]
  } deriving (Show, Eq)

-- Initial empty state.
emptyState :: ForthState
emptyState = ForthState [] Map.empty

-- Returns the stack as a list, with the top element being the last.
toList :: ForthState -> [Int]
toList = reverse . stack

-- Evaluates a Text input, updating the ForthState.
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text initialState = eval (T.words text) initialState

-- Main evaluation function, processes a list of words.
eval :: [Text] -> ForthState -> Either ForthError ForthState
eval [] state = Right state
eval (w:ws) state
  | w == ":" = defineWord ws state
  | otherwise = case processWord w state of
      Left err -> Left err
      Right newState -> eval ws newState

-- Parses and stores a new word definition.
defineWord :: [Text] -> ForthState -> Either ForthError ForthState
defineWord [] _ = Left InvalidWord -- Ran out of tokens during definition
defineWord (name:rest) state
  | isNumber name = Left InvalidWord -- Cannot redefine a number
  | otherwise =
      let (definition, remaining) = break (== ";") rest
      in if null remaining
         then Left InvalidWord -- Missing semicolon
         else
           let lowerName = T.toLower name
               newDefs = Map.insert lowerName definition (definitions state)
               newState = state { definitions = newDefs }
           in eval (tail remaining) newState -- Continue processing after semicolon

-- Processes a single word (number, built-in, or user-defined).
processWord :: Text -> ForthState -> Either ForthError ForthState
processWord word state =
  let lowerWord = T.toLower word
  in case parseNumber word of
    Just n -> Right $ state { stack = n : stack state } -- Push number onto stack
    Nothing -> case Map.lookup lowerWord builtInWords of
      Just action -> action state -- Execute built-in word
      Nothing -> case Map.lookup lowerWord (definitions state) of
        Just definition -> eval definition state -- Execute user-defined word
        Nothing -> Left (UnknownWord word) -- Word not found

-- Helper to parse a Text token as an Int.
parseNumber :: Text -> Maybe Int
parseNumber t = case TR.signed TR.decimal t of
  Right (n, "") -> Just n -- Ensure the entire token is consumed
  _             -> Nothing

-- Helper to check if a Text token represents a number.
isNumber :: Text -> Bool
isNumber t = case parseNumber t of
               Just _ -> True
               Nothing -> False

-- Map of built-in word names to their corresponding actions.
builtInWords :: Map Text (ForthState -> Either ForthError ForthState)
builtInWords = Map.fromList
  [ ("+", binaryOp (+))
  , ("-", binaryOp (-))
  , ("*", binaryOp (*))
  , ("/", safeDiv)
  , ("dup", dupOp)
  , ("drop", dropOp)
  , ("swap", swapOp)
  , ("over", overOp)
  ]

-- Helper for binary arithmetic operations.
binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state = case stack state of
  (y:x:xs) -> Right $ state { stack = op x y : xs }
  _        -> Left StackUnderflow

-- Safe division operation, checks for division by zero.
safeDiv :: ForthState -> Either ForthError ForthState
safeDiv state = case stack state of
  []      -> Left StackUnderflow
  [_]     -> Left StackUnderflow
  (0:_:_) -> Left DivisionByZero
  (y:x:xs) -> Right $ state { stack = x `div` y : xs }

-- DUP operation: duplicates the top stack element.
dupOp :: ForthState -> Either ForthError ForthState
dupOp state = case stack state of
  (x:xs) -> Right $ state { stack = x : x : xs }
  _      -> Left StackUnderflow

-- DROP operation: removes the top stack element.
dropOp :: ForthState -> Either ForthError ForthState
dropOp state = case stack state of
  (_:xs) -> Right $ state { stack = xs }
  _      -> Left StackUnderflow

-- SWAP operation: swaps the top two stack elements.
swapOp :: ForthState -> Either ForthError ForthState
swapOp state = case stack state of
  (y:x:xs) -> Right $ state { stack = x : y : xs }
  _        -> Left StackUnderflow

-- OVER operation: copies the second element from the top onto the top.
overOp :: ForthState -> Either ForthError ForthState
overOp state = case stack state of
  (y:x:xs) -> Right $ state { stack = x : y : x : xs }
  _        -> Left StackUnderflow
