{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import           Data.Char  (isDigit)
import           Data.List  (lookup)
import           Data.Text  (Text)
import qualified Data.Text  as T

-- | Possible evaluation errors.
data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- | The evaluator state: a stack and a dictionary of user‑defined words.
--   Internally the head of the list is the *top* of the stack.  This
--   makes stack manipulation operations cheap; we flip it back in
--   'toList' so the user sees the top on the right.
data ForthState = ForthState
  { stack :: [Int]                -- ^ top of stack is at the head
  , dict  :: [(Text, [Text])]     -- ^ simple association list for user words
  } deriving (Show, Eq)

------------------------------------------------------------------------
-- Public interface
------------------------------------------------------------------------

-- | An empty evaluator state.
emptyState :: ForthState
emptyState = ForthState [] []

-- | Evaluate some input text in the given state.
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText input = evalTokens (tokenize input)

-- | Return the current stack, with the /top/ element right‑most.
toList :: ForthState -> [Int]
toList (ForthState s _) = reverse s

------------------------------------------------------------------------
-- Tokenisation helpers
------------------------------------------------------------------------

-- Simple white‑space split is enough for the Exercism requirements.
tokenize :: Text -> [Text]
tokenize = T.words

------------------------------------------------------------------------
-- Core evaluation
------------------------------------------------------------------------

-- Evaluate a list of tokens.
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] st = Right st
evalTokens (tok:rest) st
  | lowerTok == ":" = defineWord rest st >>= uncurry evalTokens
  | otherwise       = processToken tok st >>= evalTokens rest
  where
    lowerTok = T.toLower tok

------------------------------------------------------------------------
-- Word definition ( : name ... ; )
------------------------------------------------------------------------

defineWord :: [Text]                  -- ^ tokens following the initial ':'
           -> ForthState
           -> Either ForthError ([Text], ForthState) -- ^ remaining tokens + new state
defineWord [] _ = Left InvalidWord
defineWord (name:body) st
  | isNumber name = Left InvalidWord
  | otherwise     =
      case span (/= ";") body of
        (_, [])               -> Left InvalidWord           -- missing ';'
        (definition, _:rest') ->
          let lcName  = T.toLower name
              defToks = map T.toLower definition
              -- replace any existing definition with the new one
              newDict = (lcName, defToks) : filter ((/= lcName) . fst) (dict st)
          in  Right (rest', st { dict = newDict })

------------------------------------------------------------------------
-- Processing individual tokens
------------------------------------------------------------------------

processToken :: Text -> ForthState -> Either ForthError ForthState
processToken tok st@(ForthState stk dct)
  | isNumber tok = Right st { stack = readInt tok : stk }
  | otherwise =
      case lookup lowTok dct of
        Just body -> evalTokens body st
        Nothing   ->
          case builtin lowTok of
            Just op -> op st
            Nothing -> Left (UnknownWord tok)
  where
    lowTok = T.toLower tok

readInt :: Text -> Int
readInt = read . T.unpack

------------------------------------------------------------------------
-- Built‑in words
------------------------------------------------------------------------

builtin :: Text -> Maybe (ForthState -> Either ForthError ForthState)
builtin "+"    = Just $ binaryOp (+)
builtin "-"    = Just $ binaryOp (-)
builtin "*"    = Just $ binaryOp (*)
builtin "/"    = Just division
builtin "dup"  = Just dup
builtin "drop" = Just drop'
builtin "swap" = Just swap
builtin "over" = Just over
builtin _      = Nothing

-- Binary operation helper (uses top as first argument).
binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp f (ForthState (x:y:xs) d) = Right $ ForthState (f y x : xs) d
binaryOp _ (ForthState _        _) = Left StackUnderflow

division :: ForthState -> Either ForthError ForthState
division (ForthState (x:y:xs) d)
  | x == 0    = Left DivisionByZero
  | otherwise = Right $ ForthState (y `quot` x : xs) d
division _ = Left StackUnderflow

dup :: ForthState -> Either ForthError ForthState
dup (ForthState (x:xs) d) = Right $ ForthState (x:x:xs) d
dup _                     = Left StackUnderflow

drop' :: ForthState -> Either ForthError ForthState
drop' (ForthState (_:xs) d) = Right $ ForthState xs d
drop' _                     = Left StackUnderflow

swap :: ForthState -> Either ForthError ForthState
swap (ForthState (x:y:xs) d) = Right $ ForthState (y:x:xs) d
swap _                       = Left StackUnderflow

over :: ForthState -> Either ForthError ForthState
over (ForthState (x:y:xs) d) = Right $ ForthState (y:x:y:xs) d
over _                       = Left StackUnderflow

------------------------------------------------------------------------
-- Misc helpers
------------------------------------------------------------------------

isNumber :: Text -> Bool
isNumber txt =
  case T.uncons txt of
    Just ('-', rest) -> not (T.null rest) && T.all isDigit rest
    _                -> T.all isDigit txt
