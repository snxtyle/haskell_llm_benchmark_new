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
import Data.Char (isDigit, isSpace)
import Data.List (foldl')

-- Define Forth errors
data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- ForthState contains a stack of integers and a dictionary of defined words
data ForthState = ForthState
  { stack :: [Int]
  , definitions :: Map.Map Text [Text]
  } deriving (Show, Eq)

-- Create an empty Forth state
emptyState :: ForthState
emptyState = ForthState [] Map.empty

-- Convert the stack to a list (top element is last)
toList :: ForthState -> [Int]
toList = reverse . stack

-- Evaluate Forth text
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input state = processTokens (tokenize input) state False []
  where
    -- Split input into tokens
    tokenize :: Text -> [Text]
    tokenize = filter (not . T.null) . map T.strip . T.split (not . isTokenChar)
    
    isTokenChar :: Char -> Bool
    isTokenChar c = not (isSpace c)
    
    -- Process tokens with state
    processTokens :: [Text] -> ForthState -> Bool -> [Text] -> Either ForthError ForthState
    processTokens [] state _ _ = Right state
    processTokens (token:tokens) state defining wordName
      | defining && token == ";" = 
          case wordName of
            [] -> Left InvalidWord
            name:[] -> processTokens tokens (addDefinition state (T.toLower name) (reverse wordName)) False []
            _ -> Left InvalidWord
      | defining = processTokens tokens state True (token:wordName)
      | token == ":" = 
          case tokens of
            [] -> Left InvalidWord
            name:rest -> processTokens rest state True [name]
      | otherwise = do
          state' <- evalToken token state
          processTokens tokens state' defining wordName
    
    -- Add a word definition to the state
    addDefinition :: ForthState -> Text -> [Text] -> ForthState
    addDefinition state name definition = 
      state { definitions = Map.insert name definition (definitions state) }
    
    -- Evaluate a single token
    evalToken :: Text -> ForthState -> Either ForthError ForthState
    evalToken token state = 
      let tokenLower = T.toLower token
      in case lookupDefinition tokenLower (definitions state) of
           Just expansion -> foldM (flip evalToken) state expansion
           Nothing -> 
             if T.all isDigit token || (T.head token == '-' && T.length token > 1 && T.all isDigit (T.drop 1 token))
             then Right $ pushToStack state (read $ T.unpack token)
             else executeBuiltIn tokenLower state
    
    -- Look up a word in definitions
    lookupDefinition :: Text -> Map.Map Text [Text] -> Maybe [Text]
    lookupDefinition word defs = Map.lookup word defs
    
    -- Push a value onto the stack
    pushToStack :: ForthState -> Int -> ForthState
    pushToStack state val = state { stack = val : stack state }
    
    -- Pop a value from the stack
    popFromStack :: ForthState -> Either ForthError (Int, ForthState)
    popFromStack state =
      case stack state of
        [] -> Left StackUnderflow
        (x:xs) -> Right (x, state { stack = xs })
    
    -- Pop two values from the stack
    popTwoFromStack :: ForthState -> Either ForthError (Int, Int, ForthState)
    popTwoFromStack state = do
      (b, state') <- popFromStack state
      (a, state'') <- popFromStack state'
      return (a, b, state'')
    
    -- Execute built-in operations
    executeBuiltIn :: Text -> ForthState -> Either ForthError ForthState
    executeBuiltIn "+" state = do
      (a, b, state') <- popTwoFromStack state
      return $ pushToStack state' (a + b)
    
    executeBuiltIn "-" state = do
      (a, b, state') <- popTwoFromStack state
      return $ pushToStack state' (a - b)
    
    executeBuiltIn "*" state = do
      (a, b, state') <- popTwoFromStack state
      return $ pushToStack state' (a * b)
    
    executeBuiltIn "/" state = do
      (a, b, state') <- popTwoFromStack state
      if b == 0
        then Left DivisionByZero
        else return $ pushToStack state' (a `div` b)
    
    executeBuiltIn "dup" state = do
      (x, state') <- popFromStack state
      let state'' = pushToStack state' x
      return $ pushToStack state'' x
    
    executeBuiltIn "drop" state = do
      (_, state') <- popFromStack state
      return state'
    
    executeBuiltIn "swap" state = do
      (a, b, state') <- popTwoFromStack state
      let state'' = pushToStack state' b
      return $ pushToStack state'' a
    
    executeBuiltIn "over" state = do
      (a, b, state') <- popTwoFromStack state
      let state'' = pushToStack state' a
      let state''' = pushToStack state'' b
      return $ pushToStack state''' a
    
    executeBuiltIn _ _ = Left InvalidWord
