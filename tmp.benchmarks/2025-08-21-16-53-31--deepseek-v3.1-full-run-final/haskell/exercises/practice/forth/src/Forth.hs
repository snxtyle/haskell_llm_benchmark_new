{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text, unpack, words, toUpper)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isDigit, isAlpha, isAlphaNum)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , definitions :: Map Text [Text]
  } deriving (Show)

emptyState :: ForthState
emptyState = ForthState { stack = [], definitions = Map.empty }

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = do
  let tokens = map toUpper <$> words text
  evalTokens tokens state

toList :: ForthState -> [Int]
toList = reverse . stack

-- Helper functions
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens (token:tokens) state
  | isNumber token = evalTokens tokens $ push (read (unpack token)) state
  | token == ":" = handleDefinition tokens state
  | otherwise = do
      case Map.lookup token (definitions state) of
        Just defTokens -> do
          newState <- evalTokens defTokens state
          evalTokens tokens newState
        Nothing -> do
          newState <- evalWord token state
          evalTokens tokens newState

isNumber :: Text -> Bool
isNumber t
  | T.null t = False
  | T.head t == '-' && T.length t > 1 = all isDigit (unpack (T.tail t))
  | otherwise = all isDigit (unpack t)

handleDefinition :: [Text] -> ForthState -> Either ForthError ForthState
handleDefinition tokens state = 
  case tokens of
    (name:rest) 
      | T.any isDigit name -> Left InvalidWord
      | otherwise -> 
          case break (== ";") rest of
            (def, []) -> Left InvalidWord  -- Missing semicolon
            (def, _:remainingTokens) -> 
                let newDefs = Map.insert (toUpper name) def (definitions state)
                in evalTokens remainingTokens state { definitions = newDefs }
    _ -> Left InvalidWord

evalWord :: Text -> ForthState -> Either ForthError ForthState
evalWord word state = case word of
  "+" -> binaryOp (+) state
  "-" -> binaryOp (-) state
  "*" -> binaryOp (*) state
  "/" -> binaryDivOp state
  "DUP" -> dup state
  "DROP" -> dropOp state
  "SWAP" -> swapOp state
  "OVER" -> overOp state
  _ -> Left (UnknownWord word)

binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state = do
  (y, state1) <- pop state
  (x, state2) <- pop state1
  Right $ push (x `op` y) state2

binaryDivOp :: ForthState -> Either ForthError ForthState
binaryDivOp state = do
  (y, state1) <- pop state
  if y == 0
    then Left DivisionByZero
    else do
      (x, state2) <- pop state1
      Right $ push (x `div` y) state2

dup :: ForthState -> Either ForthError ForthState
dup state = do
  (x, state1) <- pop state
  Right $ push x (push x state1)

dropOp :: ForthState -> Either ForthError ForthState
dropOp state = do
  (_, state1) <- pop state
  Right state1

swapOp :: ForthState -> Either ForthError ForthState
swapOp state = do
  (y, state1) <- pop state
  (x, state2) <- pop state1
  Right $ push y (push x state2)

overOp :: ForthState -> Either ForthError ForthState
overOp state = do
  (y, state1) <- pop state
  (x, state2) <- pop state1
  Right $ push x (push y (push x state2))

pop :: ForthState -> Either ForthError (Int, ForthState)
pop state =
  case stack state of
    [] -> Left StackUnderflow
    (x:xs) -> Right (x, state { stack = xs })

push :: Int -> ForthState -> ForthState
push x state = state { stack = x : stack state }
