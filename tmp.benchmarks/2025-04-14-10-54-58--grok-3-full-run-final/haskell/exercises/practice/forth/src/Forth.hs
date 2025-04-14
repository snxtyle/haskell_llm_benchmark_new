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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (toUpper)
import Control.Monad (foldM)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , dictionary :: Map Text [Text]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState
  { stack = []
  , dictionary = Map.empty
  }

toList :: ForthState -> [Int]
toList = stack

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input state = do
  let tokens = tokenize input
  if isDefinition tokens
    then handleDefinition tokens state
    else foldM evalToken state tokens

tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . T.words

isDefinition :: [Text] -> Bool
isDefinition (":":_) = True
isDefinition _ = False

handleDefinition :: [Text] -> ForthState -> Either ForthError ForthState
handleDefinition tokens state =
  case tokens of
    (":":word:rest) ->
      if any (== ";") rest
        then let definition = takeWhile (/= ";") rest
                 normalizedWord = T.toUpper word
             in Right $ state { dictionary = Map.insert normalizedWord definition (dictionary state) }
        else Left InvalidWord
    _ -> Left InvalidWord

evalToken :: ForthState -> Text -> Either ForthError ForthState
evalToken state token =
  let upperToken = T.toUpper token
  in case upperToken of
    "+" -> binaryOp (+) state
    "-" -> binaryOp (-) state
    "*" -> binaryOp (*) state
    "/" -> binaryOp div state
    "DUP" -> dup state
    "DROP" -> dropStack state
    "SWAP" -> swap state
    "OVER" -> over state
    _ -> case Map.lookup upperToken (dictionary state) of
           Just def -> foldM evalToken state def
           Nothing -> if isNumber token
                        then Right $ push (read $ T.unpack token) state
                        else Left $ UnknownWord token

isNumber :: Text -> Bool
isNumber t = case T.uncons t of
  Just ('-', rest) -> not (T.null rest) && T.all (\c -> c >= '0' && c <= '9') rest
  _ -> not (T.null t) && T.all (\c -> c >= '0' && c <= '9') t

push :: Int -> ForthState -> ForthState
push x state = state { stack = x : stack state }

pop :: ForthState -> Either ForthError (Int, ForthState)
pop state = case stack state of
  (x:xs) -> Right (x, state { stack = xs })
  [] -> Left StackUnderflow

binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state = do
  (a, state1) <- pop state
  (b, state2) <- pop state1
  if op == div && a == 0
    then Left DivisionByZero
    else Right $ push (b `op` a) state2

dup :: ForthState -> Either ForthError ForthState
dup state = do
  (x, _) <- pop state
  Right $ push x $ push x state

dropStack :: ForthState -> Either ForthError ForthState
dropStack state = do
  (_, newState) <- pop state
  Right newState

swap :: ForthState -> Either ForthError ForthState
swap state = case stack state of
  (x:y:xs) -> Right $ state { stack = y:x:xs }
  _ -> Left StackUnderflow

over :: ForthState -> Either ForthError ForthState
over state = case stack state of
  (_:y:xs) -> Right $ state { stack = y:_:y:xs }
  _ -> Left StackUnderflow
