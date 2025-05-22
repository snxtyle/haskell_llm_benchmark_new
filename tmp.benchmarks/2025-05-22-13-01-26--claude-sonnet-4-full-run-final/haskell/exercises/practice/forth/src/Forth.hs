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
import qualified Data.Map as Map
import Data.Char (toLower, isDigit)
import Data.List (foldl')

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , dictionary :: Map.Map Text [Text]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] Map.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = evalTokens (tokenize text) state

toList :: ForthState -> [Int]
toList = stack

-- Tokenize input text into words
tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . T.words . T.toLower

-- Evaluate a list of tokens
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens tokens state = case tokens of
  (":":rest) -> case parseDefinition rest of
    Left err -> Left err
    Right (name, definition, remaining) -> 
      let newDict = Map.insert name definition (dictionary state)
          newState = state { dictionary = newDict }
      in evalTokens remaining newState
  (token:rest) -> do
    newState <- evalToken token state
    evalTokens rest newState

-- Parse a word definition (: name definition ;)
parseDefinition :: [Text] -> Either ForthError (Text, [Text], [Text])
parseDefinition [] = Left InvalidWord
parseDefinition (name:rest) = 
  if isNumber name
    then Left InvalidWord
    else case break (== ";") rest of
      (_, []) -> Left InvalidWord
      (definition, (";":remaining)) -> Right (name, definition, remaining)
      _ -> Left InvalidWord

-- Check if a text represents a number
isNumber :: Text -> Bool
isNumber t = case T.uncons t of
  Nothing -> False
  Just ('-', rest) -> not (T.null rest) && T.all isDigit rest
  Just (c, rest) -> isDigit c && T.all isDigit rest

-- Parse a number from text
parseNumber :: Text -> Maybe Int
parseNumber t = case T.uncons t of
  Nothing -> Nothing
  Just ('-', rest) -> if T.null rest || not (T.all isDigit rest)
                      then Nothing
                      else Just (negate (read (T.unpack rest)))
  Just (c, rest) -> if isDigit c && T.all isDigit rest
                    then Just (read (T.unpack t))
                    else Nothing

-- Evaluate a single token
evalToken :: Text -> ForthState -> Either ForthError ForthState
evalToken token state
  | Just n <- parseNumber token = Right $ state { stack = n : stack state }
  | otherwise = case token of
      "+" -> binaryOp (+) state
      "-" -> binaryOp (-) state
      "*" -> binaryOp (*) state
      "/" -> divOp state
      "dup" -> dupOp state
      "drop" -> dropOp state
      "swap" -> swapOp state
      "over" -> overOp state
      _ -> case Map.lookup token (dictionary state) of
        Just definition -> evalTokens definition state
        Nothing -> Left (UnknownWord token)

-- Binary arithmetic operation
binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state = case stack state of
  (b:a:rest) -> Right $ state { stack = op a b : rest }
  _ -> Left StackUnderflow

-- Division operation (with zero check)
divOp :: ForthState -> Either ForthError ForthState
divOp state = case stack state of
  (0:_:_) -> Left DivisionByZero
  (b:a:rest) -> Right $ state { stack = a `div` b : rest }
  _ -> Left StackUnderflow

-- DUP operation: duplicate top element
dupOp :: ForthState -> Either ForthError ForthState
dupOp state = case stack state of
  (a:rest) -> Right $ state { stack = a : a : rest }
  [] -> Left StackUnderflow

-- DROP operation: remove top element
dropOp :: ForthState -> Either ForthError ForthState
dropOp state = case stack state of
  (_:rest) -> Right $ state { stack = rest }
  [] -> Left StackUnderflow

-- SWAP operation: swap top two elements
swapOp :: ForthState -> Either ForthError ForthState
swapOp state = case stack state of
  (a:b:rest) -> Right $ state { stack = b : a : rest }
  _ -> Left StackUnderflow

-- OVER operation: copy second element to top
overOp :: ForthState -> Either ForthError ForthState
overOp state = case stack state of
  (a:b:rest) -> Right $ state { stack = b : a : b : rest }
  _ -> Left StackUnderflow
