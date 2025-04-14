{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (words)

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text, pack, unpack, toLower, words)
import Control.Monad (foldM)
import Text.Read (readMaybe)  -- For parsing numbers

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState { stack :: [Int], dictionary :: [(Text, [Text])] }

emptyState :: ForthState
emptyState = ForthState [] []  -- Empty stack and empty dictionary

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = processTokens (words text) state

-- Helper to process a list of tokens
processTokens :: [Text] -> ForthState -> Either ForthError ForthState
processTokens tokens state = go tokens state
  where
    go [] s = Right s
    go (tok:toks) s
      | tok == ":" = case findDefinition toks of  -- Handle word definition
          Just (wordName, definition, rest) ->
            let lowerWordName = toLower wordName
                newDict = (lowerWordName, definition) : dictionary s  -- Add to dictionary, lowercased
            in go rest (s { dictionary = newDict })
          Nothing -> Left InvalidWord  -- No matching ';' found
      | otherwise = processSingleToken s tok >>= \newS -> go toks newS

    -- Extract definition: next token is word name, then until ';'
    findDefinition :: [Text] -> Maybe (Text, [Text], [Text])
    findDefinition (wordName:rest) = go [] rest
      where
        go acc (sem:xs) | sem == pack ";" = Just (wordName, reverse acc, xs)
        go acc (x:xs) = go (x:acc) xs
        go _ [] = Nothing
    findDefinition _ = Nothing  -- Not enough tokens

-- Process a single token (not part of a definition)
processSingleToken :: ForthState -> Text -> Either ForthError ForthState
processSingleToken s tok =
  case readMaybe (unpack tok) of  -- Try to parse as integer
    Just n -> Right (s { stack = n : stack s })  -- Push to stack (head is top)
    Nothing ->
      let lowerTok = toLower tok  -- Make case-insensitive
      in case lowerTok of
           t | t == pack "+" -> applyBinary (+) s
             | t == pack "-" -> applyBinary (-) s
             | t == pack "*" -> applyBinary (*) s
             | t == pack "/" -> divideStack s  -- Use separate function for division
             | t == pack "dup" -> dupStack s
             | t == pack "drop" -> dropStack s
             | t == pack "swap" -> swapStack s
             | t == pack "over" -> overStack s
             _ -> case lookup lowerTok (dictionary s) of
                    Just definition -> processTokens definition s  -- Evaluate defined word
                    Nothing -> Left (UnknownWord lowerTok)

-- Helper for binary operations (excluding division)
applyBinary :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
applyBinary op s =
  case stack s of
    (b:a:rest) -> Right (s { stack = (op a b) : rest })  -- Apply op to deeper (a) and top (b)
    _ -> Left StackUnderflow

-- Specific function for division
divideStack :: ForthState -> Either ForthError ForthState
divideStack s =
  case stack s of
    (b:a:rest) ->
      if b == 0  -- Check for division by zero
        then Left DivisionByZero
        else Right (s { stack = (a `div` b) : rest })  -- Divide deeper (a) by top (b)
    _ -> Left StackUnderflow

-- Stack manipulation functions
dupStack :: ForthState -> Either ForthError ForthState
dupStack s =
  case stack s of
    (x:xs) -> Right (s { stack = x : x : xs })  -- Duplicate top
    _ -> Left StackUnderflow

dropStack :: ForthState -> Either ForthError ForthState
dropStack s =
  case stack s of
    (_:xs) -> Right (s { stack = xs })  -- Remove top
    _ -> Left StackUnderflow

swapStack :: ForthState -> Either ForthError ForthState
swapStack s =
  case stack s of
    (a:b:xs) -> Right (s { stack = b : a : xs })  -- Swap top two
    _ -> Left StackUnderflow

overStack :: ForthState -> Either ForthError ForthState
overStack s =
  case stack s of
    (a:b:xs) -> Right (s { stack = b : a : b : xs })  -- Duplicate second top
    _ -> Left StackUnderflow

toList :: ForthState -> [Int]
toList s = reverse (stack s)  -- Reverse to make top (head) the rightmost element
