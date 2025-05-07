{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import           Data.Char                      ( isDigit )
import           Data.Map.Strict                ( Map )
import qualified Data.Map.Strict               as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- Stack is a list of Int, head is the top of the stack.
-- Dictionary maps lowercase word names to their definition token lists (all lowercase).
data ForthState = ForthState
  { dict  :: Map Text [Text]
  , stack :: [Int]
  }

emptyState :: ForthState
emptyState = ForthState Map.empty []

toList :: ForthState -> [Int]
toList (ForthState _ st) = reverse st

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input = evalTokens (T.words input)

-- Evaluate a list of tokens
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] st = Right st
evalTokens (t:ts) st
  | lc == ":" = parseDefinition ts st >>= \(st', rest) -> evalTokens rest st'
  | otherwise = processWord t st >>= \st' -> evalTokens ts st'
  where
    lc = T.toLower t

-- Parse a definition: ":" name body... ";" 
-- Returns updated state and the remaining tokens after the ";"
parseDefinition
  :: [Text]
  -> ForthState
  -> Either ForthError (ForthState, [Text])
parseDefinition [] _ = Left InvalidWord
parseDefinition (name:rest) (ForthState d s)
  | T.all isDigit nameLower = Left InvalidWord
  | otherwise = case break (\w -> T.toLower w == ";") rest of
    (body, semicolon:after) ->
      let lowerName = nameLower
          -- store the body tokens in lowercase for case-insensitive lookup
          lowerBody = map T.toLower body
          newDict    = Map.insert lowerName lowerBody d
      in  Right (ForthState newDict s, after)
    _ -> Left InvalidWord
 where
  nameLower = T.toLower name

-- Process a single token (non-definition)
processWord :: Text -> ForthState -> Either ForthError ForthState
processWord t st@(ForthState d stk)
  | T.all isDigit t = Right $ ForthState d (read (T.unpack t) : stk)
  | otherwise = case T.toLower t of
    "+" -> binOp (+) st
    "-" -> binOp (-) st
    "*" -> binOp (*) st
    "/" -> safeDiv st
    "dup"  -> unaryOp dupOp st
    "drop" -> unaryOp dropOp st
    "swap" -> unaryOp swapOp st
    "over" -> unaryOp overOp st
    w | Just body <- Map.lookup w d -> evalTokens body st
      | otherwise -> Left (UnknownWord t)

-- Helpers for built-ins
unaryOp
  :: ( [Int] -> Either ForthError [Int] )
  -> ForthState
  -> Either ForthError ForthState
unaryOp op (ForthState d stk) = op stk >>= \newStk -> Right (ForthState d newStk)

binOp
  :: (Int -> Int -> Int)
  -> ForthState
  -> Either ForthError ForthState
binOp f (ForthState d stk) = case stk of
  x : y : rest -> Right $ ForthState d (f y x : rest)
  _            -> Left StackUnderflow

safeDiv :: ForthState -> Either ForthError ForthState
safeDiv (ForthState d stk) = case stk of
  0 : _ -> Left DivisionByZero
  x : y : rest -> Right $ ForthState d (y `div` x : rest)
  _            -> Left StackUnderflow

dupOp :: [Int] -> Either ForthError [Int]
dupOp (x:xs) = Right (x:x:xs)
dupOp _      = Left StackUnderflow

dropOp :: [Int] -> Either ForthError [Int]
dropOp (_:xs) = Right xs
dropOp _      = Left StackUnderflow

swapOp :: [Int] -> Either ForthError [Int]
swapOp (x:y:rest) = Right (y:x:rest)
swapOp _          = Left StackUnderflow

overOp :: [Int] -> Either ForthError [Int]
overOp (x:y:rest) = Right (y:x:y:rest)
overOp _          = Left StackUnderflow
