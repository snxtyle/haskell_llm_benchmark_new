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
import Data.Char (isDigit)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data Token = TNumber Integer | TWord Text deriving (Eq, Show)

data ForthState = ForthState
  { stack :: [Integer]
  , dict :: [(Text, [Token])]
  } deriving (Eq, Show)

emptyState :: ForthState
emptyState = ForthState [] []

toList :: ForthState -> [Int]
toList state = map fromInteger (reverse (stack state))

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = evalTokens (tokenize text) state
  where
    tokenize = map token . T.words
    token t
      | T.all isDigit t = TNumber (read (T.unpack t))
      | otherwise = TWord (T.toUpper t)

evalTokens :: [Token] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens (TNumber n : tokens) state = 
  evalTokens tokens (state { stack = n : stack state })
evalTokens (TWord w : tokens) state
  | w == ":"  = define tokens state
  | isBuiltIn w = case execBuiltIn w state of
                    Left e -> Left e
                    Right state' -> evalTokens tokens state'
  | otherwise = case lookup w (dict state) of
                  Just def -> evalTokens def state >>= \s -> evalTokens tokens s
                  Nothing  -> Left (UnknownWord w)
  where
    isBuiltIn word = word `elem` ["+", "-", "*", "/", "DUP", "DROP", "SWAP", "OVER"]

define :: [Token] -> ForthState -> Either ForthError ForthState
define tokens state = case tokens of
  [] -> Left InvalidWord
  (TNumber _ : _) -> Left InvalidWord
  (TWord name : rest) -> 
    case break isSemicolon rest of
      (_, []) -> Left InvalidWord
      (def, _ : rest') -> 
        let newState = state { dict = (name, def) : dict state }
        in evalTokens rest' newState
  where
    isSemicolon (TWord s) = s == ";"
    isSemicolon _ = False

execBuiltIn :: Text -> ForthState -> Either ForthError ForthState
execBuiltIn "+"  (ForthState s d) = case s of
  x:y:xs -> Right (ForthState (y+x:xs) d)
  _       -> Left StackUnderflow
execBuiltIn "-"  (ForthState s d) = case s of
  x:y:xs -> Right (ForthState (y-x:xs) d)
  _       -> Left StackUnderflow
execBuiltIn "*"  (ForthState s d) = case s of
  x:y:xs -> Right (ForthState (y*x:xs) d)
  _       -> Left StackUnderflow
execBuiltIn "/"  (ForthState s d) = case s of
  x:y:xs -> 
    if x == 0 
    then Left DivisionByZero 
    else Right (ForthState (y `quot` x:xs) d)
  _ -> Left StackUnderflow
execBuiltIn "DUP" (ForthState s d) = case s of
  []    -> Left StackUnderflow
  (x:xs) -> Right (ForthState (x:x:xs) d)
execBuiltIn "DROP" (ForthState s d) = case s of
  []    -> Left StackUnderflow
  (_:xs) -> Right (ForthState xs d)
execBuiltIn "SWAP" (ForthState s d) = case s of
  x:y:xs -> Right (ForthState (y:x:xs) d)
  _       -> Left StackUnderflow
execBuiltIn "OVER" (ForthState s d) = case s of
  x:y:xs -> Right (ForthState (y:x:y:xs) d)
  _       -> Left StackUnderflow
execBuiltIn _   s       = Right s
