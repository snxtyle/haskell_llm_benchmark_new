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
import qualified Data.Map as M
import Data.Char (isDigit, toLower)
import Text.Read (readMaybe)

-- Represents a Forth word, which can be a number or a string (for built-in or user-defined words)
data ForthToken = TokenNumber Int | TokenWord Text deriving (Show, Eq)

-- ForthState holds the data stack and the dictionary of user-defined words.
-- The stack is a list of Int, where the head is the top of the stack.
-- The dictionary maps Text (word name, lowercase) to a list of ForthToken (its definition).
data ForthState = ForthState
  { stack :: [Int]
  , dictionary :: M.Map Text [ForthToken]
  } deriving (Show, Eq)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- Initializes an empty ForthState.
emptyState :: ForthState
emptyState = ForthState
  { stack = []
  , dictionary = M.empty
  }

-- Returns the current stack as a list, with the top of the stack being the rightmost (last) element.
toList :: ForthState -> [Int]
toList state = reverse (stack state)

-- Parses a single word from the input text into a ForthToken.
-- Words are converted to lowercase for case-insensitivity.
parseWord :: Text -> ForthToken
parseWord w = case readMaybe (T.unpack w) of
  Just n -> TokenNumber n
  Nothing -> TokenWord (T.toLower w)

-- Tokenizes the input text into a list of ForthTokens.
tokenize :: Text -> [ForthToken]
tokenize input = map parseWord . T.words $ input

-- Checks if a string can be parsed as an integer.
isNumber :: String -> Bool
isNumber s = case readMaybe s :: Maybe Int of
  Just _ -> True
  Nothing -> False

-- Main evaluation function. Takes input text and initial state, returns new state or an error.
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input initialState = evalTokens (tokenize input) initialState

-- Recursively evaluates a list of ForthTokens.
evalTokens :: [ForthToken] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens (token:restTokens) state =
  case token of
    TokenNumber n -> evalTokens restTokens (state { stack = n : stack state })
    TokenWord ":" ->
      case restTokens of
        (TokenWord newWordName : definitionTokens) ->
          -- Check if newWordName is a valid word name (cannot be a number)
          if isNumber (T.unpack newWordName)
             then Left InvalidWord
             else
               let (def, remaining) = takeDefinition definitionTokens []
               in if null def || T.null newWordName -- Definition must not be empty and word name must not be empty
                  then Left InvalidWord
                  else do
                    let newDict = M.insert newWordName def (dictionary state)
                    evalTokens remaining (state { dictionary = newDict })
        _ -> Left InvalidWord -- Malformed definition (e.g., ": ;" or ": 1 ;")
    TokenWord ";" -> Left InvalidWord -- Should not be encountered directly outside a definition
    TokenWord word ->
      case M.lookup word (dictionary state) of
        Just definitionTokens -> do
          -- Execute user-defined word, then continue with remaining tokens.
          stateAfterDef <- evalTokens definitionTokens state
          evalTokens restTokens stateAfterDef
        Nothing -> do
          -- Try built-in word, then continue with remaining tokens.
          stateAfterBuiltin <- evalBuiltinWord word state
          evalTokens restTokens stateAfterBuiltin

-- Helper to extract definition tokens until a ';' is found.
takeDefinition :: [ForthToken] -> [ForthToken] -> ([ForthToken], [ForthToken])
takeDefinition [] acc = (reverse acc, []) -- Reached end of input without ';'
takeDefinition (TokenWord ";" : rest) acc = (reverse acc, rest)
takeDefinition (t : ts) acc = takeDefinition ts (t : acc)

-- Evaluates built-in Forth words.
evalBuiltinWord :: Text -> ForthState -> Either ForthError ForthState
evalBuiltinWord word state =
  let s = stack state
  in case word of
    "+" -> binOp False (+) s state
    "-" -> binOp False (-) s state
    "*" -> binOp False (*) s state
    "/" -> binOp True div s state
    "dup" -> unaryOp (\x -> [x, x]) s state
    "drop" -> unaryOp (const []) s state
    "swap" -> stackOp2 (\x y -> [y, x]) s state -- x is second, y is top. Result: y on top, x second.
    "over" -> stackOp2 (\x y -> [x, y, x]) s state -- x is second, y is top. Result: x on top, y second, x third.
    _ -> Left (UnknownWord word)

-- Helper for binary arithmetic operations (e.g., +, -, *, /).
-- Pops two elements (y then x), pushes result (x op y).
binOp :: Bool -> (Int -> Int -> Int) -> [Int] -> ForthState -> Either ForthError ForthState
binOp isDiv op (y:x:xs) state
  | isDiv && y == 0 = Left DivisionByZero
  | otherwise = Right $ state { stack = op x y : xs }
binOp _ _ _ = Left StackUnderflow

-- Helper for unary stack operations (e.g., DUP, DROP).
-- Pops one element (x), pushes new elements based on f.
unaryOp :: (Int -> [Int]) -> [Int] -> ForthState -> Either ForthError ForthState
unaryOp f (x:xs) state = Right $ state { stack = f x ++ xs }
unaryOp _ _ _ = Left StackUnderflow

-- Helper for binary stack operations (e.g., SWAP, OVER).
-- Pops two elements (y then x), pushes new elements based on f.
stackOp2 :: (Int -> Int -> [Int]) -> [Int] -> ForthState -> Either ForthError ForthState
stackOp2 f (y:x:xs) state = Right $ state { stack = f x y ++ xs }
stackOp2 _ _ _ = Left StackUnderflow
