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
import Data.Char (isDigit, toLower)
import Data.Map (Map)
import qualified Data.Map as Map

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , definitions :: Map Text [Text]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] Map.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = evalTokens (tokenize text) state

toList :: ForthState -> [Int]
toList = stack

-- Tokenize the input text
tokenize :: Text -> [Text]
tokenize = map T.toLower . T.words

-- Evaluate a list of tokens
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens (":":rest) state = case parseDefinition rest of
  Just (name, body, remaining) -> 
    if isNumber name
      then Left InvalidWord
      else evalTokens remaining (state { definitions = Map.insert name body (definitions state) })
  Nothing -> Left InvalidWord
evalTokens (token:rest) state = do
  newState <- evalToken token state
  evalTokens rest newState

-- Parse a word definition
parseDefinition :: [Text] -> Maybe (Text, [Text], [Text])
parseDefinition tokens = go [] tokens
  where
    go _ [] = Nothing
    go _ [_] = Nothing
    go acc (";":rest) = case reverse acc of
      [] -> Nothing
      (name:body) -> Just (name, reverse body, rest)
    go acc (t:ts) = go (t:acc) ts

-- Evaluate a single token
evalToken :: Text -> ForthState -> Either ForthError ForthState
evalToken token state
  | isNumber token = Right $ pushNumber (read $ T.unpack token) state
  | token `Map.member` definitions state = 
      case Map.lookup token (definitions state) of
        Just body -> evalTokens body state
        Nothing -> Left (UnknownWord token)
  | otherwise = evalBuiltin token state

-- Check if a token is a number
isNumber :: Text -> Bool
isNumber t = case T.unpack t of
  [] -> False
  ('-':xs) -> not (null xs) && all isDigit xs
  xs -> all isDigit xs

-- Push a number onto the stack
pushNumber :: Int -> ForthState -> ForthState
pushNumber n state = state { stack = n : stack state }

-- Evaluate built-in operations
evalBuiltin :: Text -> ForthState -> Either ForthError ForthState
evalBuiltin "+" state = binaryOp (+) state
evalBuiltin "-" state = binaryOp (-) state
evalBuiltin "*" state = binaryOp (*) state
evalBuiltin "/" state = divOp state
evalBuiltin "dup" state = dupOp state
evalBuiltin "drop" state = dropOp state
evalBuiltin "swap" state = swapOp state
evalBuiltin "over" state = overOp state
evalBuiltin word _ = Left (UnknownWord word)

-- Binary arithmetic operation
binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state = case stack state of
  (y:x:xs) -> Right $ state { stack = op x y : xs }
  _ -> Left StackUnderflow

-- Division operation (checks for division by zero)
divOp :: ForthState -> Either ForthError ForthState
divOp state = case stack state of
  (0:_:_) -> Left DivisionByZero
  (y:x:xs) -> Right $ state { stack = x `div` y : xs }
  _ -> Left StackUnderflow

-- DUP: Duplicate the top element
dupOp :: ForthState -> Either ForthError ForthState
dupOp state = case stack state of
  (x:xs) -> Right $ state { stack = x:x:xs }
  _ -> Left StackUnderflow

-- DROP: Remove the top element
dropOp :: ForthState -> Either ForthError ForthState
dropOp state = case stack state of
  (_:xs) -> Right $ state { stack = xs }
  _ -> Left StackUnderflow

-- SWAP: Swap the top two elements
swapOp :: ForthState -> Either ForthError ForthState
swapOp state = case stack state of
  (y:x:xs) -> Right $ state { stack = x:y:xs }
  _ -> Left StackUnderflow

-- OVER: Copy the second element to the top
overOp :: ForthState -> Either ForthError ForthState
overOp state = case stack state of
  (y:x:xs) -> Right $ state { stack = x:y:x:xs }
  _ -> Left StackUnderflow
