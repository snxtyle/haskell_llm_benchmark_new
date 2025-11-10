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
import Data.Char (isDigit)
import Control.Monad (when)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- | ForthState holds the data stack and a dictionary of user-defined words.
-- The stack is a list of Ints, where the head of the list is the top of the stack.
-- The dictionary maps a word name (Text) to its definition (a list of Text tokens).
data ForthState = ForthState
  { stack :: [Int]
  , dict  :: Map Text [Text]
  } deriving (Show, Eq)

-- | Returns an empty ForthState with an empty stack and dictionary.
emptyState :: ForthState
emptyState = ForthState { stack = [], dict = Map.empty }

-- | Evaluates a line of Forth code, updating the state.
-- It processes tokens one by one, handling numbers, built-in words,
-- and user-defined word definitions.
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state =
  let tokens = T.words (T.toUpper text)
  in evalTokens tokens state

-- | Helper function to evaluate a list of tokens.
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] s = Right s
evalTokens (t:ts) s
  | isNumber t = let n = read (T.unpack t) :: Int
                 in evalTokens ts (s { stack = n : stack s })
  | otherwise = case t of
      ":" -> handleDefinition ts s
      "+"  -> applyBinaryOp (+) ts s
      "-"  -> applyBinaryOp (-) ts s
      "*"  -> applyBinaryOp (*) ts s
      "/"  -> applyDivision ts s
      "DUP" -> applyStackOp (\(x:xs) -> (x:x:xs)) ts s
      "DROP"-> applyStackOp (\(_:xs) -> xs) ts s
      "SWAP" -> applyStackOp (\(y:x:xs) -> (x:y:xs)) ts s
      "OVER" -> applyStackOp (\(y:x:xs) -> (x:y:x:xs)) ts s
      _     -> handleUserWord t ts s

-- | Checks if a Text token represents a number.
isNumber :: Text -> Bool
isNumber = T.all isDigit

-- | Handles the definition of a new word, e.g., ": MY-SWAP SWAP ;".
handleDefinition :: [Text] -> ForthState -> Either ForthError ForthState
handleDefinition [] _ = Left InvalidWord
handleDefinition (name:rest) s
  | isNumber name = Left InvalidWord
  | otherwise =
      let (defTokens, afterSemi) = break (== ";") rest
      in case afterSemi of
           [] -> Left InvalidWord
           (";":remainingInput) ->
             let newDef = map T.toUpper defTokens
                 newDict = Map.insert (T.toUpper name) newDef (dict s)
                 newState = s { dict = newDict }
             in evalTokens remainingInput newState
           _ -> Left InvalidWord -- Should not happen with break

-- | Handles execution of a user-defined word.
handleUserWord :: Text -> [Text] -> ForthState -> Either ForthError ForthState
handleUserWord word ts s =
  case Map.lookup word (dict s) of
    Nothing -> Left (UnknownWord word)
    Just definition -> do
      s' <- evalTokens definition s
      evalTokens ts s'

-- | Applies a binary arithmetic operation to the top two stack items.
applyBinaryOp :: (Int -> Int -> Int) -> [Text] -> ForthState -> Either ForthError ForthState
applyBinaryOp op ts s =
  case stack s of
    (y:x:xs) -> evalTokens ts (s { stack = op x y : xs })
    _ -> Left StackUnderflow

-- | Applies integer division, handling DivisionByZero.
applyDivision :: [Text] -> ForthState -> Either ForthError ForthState
applyDivision ts s =
  case stack s of
    (y:x:xs)
      | y == 0 -> Left DivisionByZero
      | otherwise -> evalTokens ts (s { stack = x `div` y : xs })
    _ -> Left StackUnderflow

-- | Applies a stack manipulation operation.
applyStackOp :: ([Int] -> [Int]) -> [Text] -> ForthState -> Either ForthError ForthState
applyStackOp op ts s =
  case stack s of
    xs@(x:_) -> evalTokens ts (s { stack = op xs })
    [] -> Left StackUnderflow

-- | Returns the current stack as a list, with the top of the stack
-- being the rightmost (last) element.
toList :: ForthState -> [Int]
toList = reverse . stack
