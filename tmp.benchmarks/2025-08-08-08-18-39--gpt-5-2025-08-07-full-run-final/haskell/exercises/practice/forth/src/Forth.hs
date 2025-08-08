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
import qualified Data.Map.Strict as M
import Data.Char (isDigit)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- Internal dictionary maps a (lowercased) word to its (flattened) token list
type Dict = M.Map Text [Text]

data ForthState = ForthState
  { fsStack :: [Int]   -- top of stack is at the head of the list
  , fsDict  :: Dict
  }

emptyState :: ForthState
emptyState = ForthState { fsStack = [], fsDict = M.empty }

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text st0 =
  processTokens (tokenize text) st0

toList :: ForthState -> [Int]
toList (ForthState s _) = reverse s

-- Tokenize input: split on whitespace and normalize to lowercase (case-insensitive language)
tokenize :: Text -> [Text]
tokenize = map T.toLower . T.words

isNumber :: Text -> Bool
isNumber t = not (T.null t) && T.all isDigit t

readNumber :: Text -> Int
readNumber = read . T.unpack

-- Main token processing loop
processTokens :: [Text] -> ForthState -> Either ForthError ForthState
processTokens [] st = Right st
processTokens (t:ts) st
  | t == ":"  = do
      (rest, st') <- parseDefinition ts st
      processTokens rest st'
  | isNumber t =
      let n = readNumber t
      in processTokens ts (st { fsStack = n : fsStack st })
  | otherwise = case t of
      "+"    -> binOp (+) ts st
      "-"    -> binOp (-) ts st
      "*"    -> binOp (*) ts st
      "/"    -> divOp ts st
      "dup"  -> dupOp ts st
      "drop" -> dropOp ts st
      "swap" -> swapOp ts st
      "over" -> overOp ts st
      _      -> case M.lookup t (fsDict st) of
                  Just body -> processTokens (body ++ ts) st
                  Nothing   -> Left (UnknownWord t)

-- Parse a definition that starts after encountering ":"
-- Returns remaining tokens after ";" and updated state
parseDefinition :: [Text] -> ForthState -> Either ForthError ([Text], ForthState)
parseDefinition [] _ = Left InvalidWord
parseDefinition (name:rest) st
  | isNumber name = Left InvalidWord
  | otherwise =
      case break (== ";") rest of
        (_, [])        -> Left InvalidWord
        (body, _:more) ->
          let flat = flattenBody (fsDict st) body
              st'  = st { fsDict = M.insert name flat (fsDict st) }
          in Right (more, st')

-- Flatten a body using the current dictionary so that redefinitions later do not affect it
flattenBody :: Dict -> [Text] -> [Text]
flattenBody _ [] = []
flattenBody d (t:ts)
  | isNumber t = t : flattenBody d ts
  | otherwise =
      case M.lookup t d of
        Just expansion -> expansion ++ flattenBody d ts
        Nothing        -> t : flattenBody d ts

-- Stack operations and arithmetic

binOp :: (Int -> Int -> Int) -> [Text] -> ForthState -> Either ForthError ForthState
binOp f ts st =
  case fsStack st of
    (x:y:xs) ->
      let r = f y x
      in processTokens ts (st { fsStack = r : xs })
    _ -> Left StackUnderflow

divOp :: [Text] -> ForthState -> Either ForthError ForthState
divOp ts st =
  case fsStack st of
    (0:_:_) -> Left DivisionByZero
    (x:y:xs) ->
      let r = y `quot` x
      in processTokens ts (st { fsStack = r : xs })
    _ -> Left StackUnderflow

dupOp :: [Text] -> ForthState -> Either ForthError ForthState
dupOp ts st =
  case fsStack st of
    (x:xs) -> processTokens ts (st { fsStack = x:x:xs })
    _      -> Left StackUnderflow

dropOp :: [Text] -> ForthState -> Either ForthError ForthState
dropOp ts st =
  case fsStack st of
    (_:xs) -> processTokens ts (st { fsStack = xs })
    _      -> Left StackUnderflow

swapOp :: [Text] -> ForthState -> Either ForthError ForthState
swapOp ts st =
  case fsStack st of
    (x:y:xs) -> processTokens ts (st { fsStack = y:x:xs })
    _        -> Left StackUnderflow

overOp :: [Text] -> ForthState -> Either ForthError ForthState
overOp ts st =
  case fsStack st of
    (x:y:xs) -> processTokens ts (st { fsStack = y:x:y:xs })
    _        -> Left StackUnderflow
