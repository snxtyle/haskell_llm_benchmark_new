{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import           Data.Char       (isDigit, toLower)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T

-------------------------------------------------------------------------------
-- Errors
-------------------------------------------------------------------------------

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-------------------------------------------------------------------------------
-- Machine state
-------------------------------------------------------------------------------

type Stack = [Int]
type Dict  = Map.Map Text [Text]  -- case-folded word name -> list of tokens

data ForthState = ForthState
  { stStack :: Stack
  , stDict  :: Dict
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] Map.empty

-------------------------------------------------------------------------------
-- Public API
-------------------------------------------------------------------------------

-- | Evaluate a piece of Forth code text in the given state.
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input st0 = processTokens (tokenize input) st0

-- | Convert the current stack to a Haskell list, with the element on
--   top of the stack appearing last in the resulting list.
toList :: ForthState -> [Int]
toList (ForthState s _) = reverse s

-------------------------------------------------------------------------------
-- Tokenisation helpers
-------------------------------------------------------------------------------

-- | Split the input on white-space.
tokenize :: Text -> [Text]
tokenize = T.words

-- | Detect whether a token represents an integer number.
isNumber :: Text -> Bool
isNumber t =
  case T.uncons t of
    Just ('-', rest) -> not (T.null rest) && T.all isDigit rest
    _                -> T.all isDigit t

-- | Convert a (non-numeric) token to lower-case for case-insensitive lookup.
canonical :: Text -> Text
canonical t
  | isNumber t = t
  | otherwise  = T.toLower (T.map toLower t)

-------------------------------------------------------------------------------
-- Main evaluation loop
-------------------------------------------------------------------------------

processTokens :: [Text] -> ForthState -> Either ForthError ForthState
processTokens []     st = Right st
processTokens (t:ts) st =
  case canonical t of
    ":" -> do                                   -- start of word definition
      (st', rest) <- defineWord ts st
      processTokens rest st'
    tok -> do                                   -- ordinary token
      st' <- executeToken tok st
      processTokens ts st'

-------------------------------------------------------------------------------
-- Word definition  ( : name ... ; )
-------------------------------------------------------------------------------

-- | Parse a word definition.
--   Returns the updated state together with the remaining tokens
--   that follow the terminating semicolon.
defineWord :: [Text]                 -- ^ tokens following the initial ':'
           -> ForthState
           -> Either ForthError (ForthState, [Text])
defineWord [] _ = Left InvalidWord
defineWord (name:rest) (ForthState stk dict)
  | isNumber name = Left InvalidWord
  | otherwise     =
      case break (== ";") rest of
        (body, [])          -> Left InvalidWord             -- missing ';'
        ([], _:_ )          -> Left InvalidWord             -- empty body
        (body, _ : after )  ->
          let body'  = map canonical body
              dict'  = Map.insert (canonical name) body' dict
          in Right (ForthState stk dict', after)

-------------------------------------------------------------------------------
-- Executing a single token
-------------------------------------------------------------------------------

executeToken :: Text -> ForthState -> Either ForthError ForthState
executeToken tok st@(ForthState stack dict)
  | isNumber tok =                                          -- push literal
      Right st { stStack = read (T.unpack tok) : stack }

  | Just def <- Map.lookup tok dict =                       -- user-defined word
      processTokens def st

  | otherwise = case tok of                                 -- built-in words
      "+"    -> binaryOp (+) st
      "-"    -> binaryOp (-) st
      "*"    -> binaryOp (*) st
      "/"    -> divisionOp st
      "dup"  -> unaryOp 1 (\[x]   -> [x, x])      st
      "drop" -> unaryOp 1 (const [])              st
      "swap" -> unaryOp 2 (\[a, b] -> [b, a])     st
      "over" -> unaryOp 2 (\[a, b] -> [b, a, b])  st
      _      -> Left (UnknownWord tok)

-------------------------------------------------------------------------------
-- Primitive helpers
-------------------------------------------------------------------------------

-- | Pop @n@ elements (ensuring availability) and push result of transformer.
unaryOp :: Int -> ([Int] -> [Int]) -> ForthState -> Either ForthError ForthState
unaryOp n f (ForthState stack dict)
  | length stack < n = Left StackUnderflow
  | otherwise        =
      let (xs, rest) = splitAt n stack
      in Right $ ForthState (f xs ++ rest) dict

-- | Binary operators that require exactly two operands.
binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op (ForthState stack dict)
  | length stack < 2 = Left StackUnderflow
  | otherwise        =
      let (y : x : rest) = stack      -- y is top of stack
      in Right $ ForthState (op x y : rest) dict

divisionOp :: ForthState -> Either ForthError ForthState
divisionOp (ForthState stack dict)
  | length stack < 2 = Left StackUnderflow
  | y == 0           = Left DivisionByZero
  | otherwise        = Right $ ForthState (x `quot` y : rest) dict
  where
    (y : x : rest) = stack
