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
import Data.Char (isDigit, toUpper)
import qualified Data.Map as Map
import Data.List (foldl')

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , definitions :: Map.Map Text [Text]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] Map.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = 
  let tokens = tokenize text
  in evalTokens tokens state

toList :: ForthState -> [Int]
toList = reverse . stack

-- Tokenize input text into words
tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . T.words

-- Check if a token is a number
isNumber :: Text -> Bool
isNumber t = case T.uncons t of
  Just ('-', rest) -> not (T.null rest) && T.all isDigit rest
  _ -> not (T.null t) && T.all isDigit t

-- Parse a number from text
parseNumber :: Text -> Int
parseNumber = read . T.unpack

-- Convert text to uppercase for case-insensitive comparison
toUpperText :: Text -> Text
toUpperText = T.map toUpper

-- Evaluate a list of tokens
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens (":":rest) state = evalDefinition rest state
evalTokens (token:rest) state = do
  newState <- evalToken token state
  evalTokens rest newState

-- Evaluate a single token
evalToken :: Text -> ForthState -> Either ForthError ForthState
evalToken token state
  | isNumber token = Right $ pushStack (parseNumber token) state
  | otherwise = evalWord (toUpperText token) state

-- Push a value onto the stack
pushStack :: Int -> ForthState -> ForthState
pushStack n state = state { stack = n : stack state }

-- Evaluate a word (built-in or custom)
evalWord :: Text -> ForthState -> Either ForthError ForthState
evalWord word state =
  case Map.lookup word (definitions state) of
    Just defTokens -> evalTokens defTokens state
    Nothing -> evalBuiltin word state

-- Evaluate built-in words
evalBuiltin :: Text -> ForthState -> Either ForthError ForthState
evalBuiltin "+" state = binaryOp (+) state
evalBuiltin "-" state = binaryOp (-) state
evalBuiltin "*" state = binaryOp (*) state
evalBuiltin "/" state = divOp state
evalBuiltin "DUP" state = dupOp state
evalBuiltin "DROP" state = dropOp state
evalBuiltin "SWAP" state = swapOp state
evalBuiltin "OVER" state = overOp state
evalBuiltin word _ = Left (UnknownWord word)

-- Binary arithmetic operation
binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state = case stack state of
  (b:a:rest) -> Right $ state { stack = op a b : rest }
  _ -> Left StackUnderflow

-- Division operation (checks for division by zero)
divOp :: ForthState -> Either ForthError ForthState
divOp state = case stack state of
  (0:_:_) -> Left DivisionByZero
  (b:a:rest) -> Right $ state { stack = a `div` b : rest }
  _ -> Left StackUnderflow

-- DUP: duplicate top of stack
dupOp :: ForthState -> Either ForthError ForthState
dupOp state = case stack state of
  (a:rest) -> Right $ state { stack = a:a:rest }
  _ -> Left StackUnderflow

-- DROP: remove top of stack
dropOp :: ForthState -> Either ForthError ForthState
dropOp state = case stack state of
  (_:rest) -> Right $ state { stack = rest }
  _ -> Left StackUnderflow

-- SWAP: swap top two elements
swapOp :: ForthState -> Either ForthError ForthState
swapOp state = case stack state of
  (b:a:rest) -> Right $ state { stack = a:b:rest }
  _ -> Left StackUnderflow

-- OVER: copy second element to top
overOp :: ForthState -> Either ForthError ForthState
overOp state = case stack state of
  (b:a:rest) -> Right $ state { stack = a:b:a:rest }
  _ -> Left StackUnderflow

-- Evaluate a word definition
evalDefinition :: [Text] -> ForthState -> Either ForthError ForthState
evalDefinition tokens state = 
  case break (== ";") tokens of
    ([], _) -> Left InvalidWord
    (_, []) -> Left InvalidWord
    (nameAndDef, _:rest) -> 
      case nameAndDef of
        [] -> Left InvalidWord
        (name:def) -> 
          if isNumber name
          then Left InvalidWord
          else 
            let upperName = toUpperText name
                expandedDef = expandDefinitions def (definitions state)
                newDefs = Map.insert upperName expandedDef (definitions state)
                newState = state { definitions = newDefs }
            in evalTokens rest newState

-- Expand custom words in a definition
expandDefinitions :: [Text] -> Map.Map Text [Text] -> [Text]
expandDefinitions tokens defs = concatMap (expandToken defs) tokens

-- Expand a single token if it's a custom word
expandToken :: Map.Map Text [Text] -> Text -> [Text]
expandToken defs token
  | isNumber token = [token]
  | otherwise = 
      case Map.lookup (toUpperText token) defs of
        Just expanded -> expanded
        Nothing -> [token]
