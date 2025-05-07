{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState -- Constructor ForthStateInternal is not exported
  , evalText
  , toList
  , emptyState
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Control.Monad (foldM)

-- Data types
data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord      -- Used for malformed definitions, or if a word name is a number.
     | UnknownWord Text -- When a word is not a number, not built-in, and not user-defined.
     deriving (Show, Eq)

-- The internal representation of ForthState.
-- fsStack: The integer stack, where the head of the list is the top of the stack.
-- fsDefinitions: A map from word names (lowercase Text) to their definitions (a list of lowercase Text tokens).
data ForthState = ForthStateInternal
  { fsStack :: [Int]
  , fsDefinitions :: Map.Map Text [Text]
  } deriving (Show, Eq) -- deriving Show and Eq for easier testing.

-- Public interface functions
emptyState :: ForthState
emptyState = ForthStateInternal [] Map.empty

toList :: ForthState -> [Int]
toList (ForthStateInternal stack _) = reverse stack -- Stack head is top; problem expects top as last element in list.

-- Core evaluation logic
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText inputText initialState =
    let tokens = T.words inputText -- Split input into space-separated tokens.
    -- mainLoop processes these tokens and returns the final state and any remaining tokens.
    -- For evalText, we expect all tokens to be consumed, so we only care about the final state.
    in fst <$> mainLoop tokens initialState

-- Helper function to parse an integer from Text.
-- Returns Nothing if the Text does not represent a valid integer.
parseInteger :: Text -> Maybe Int
parseInteger t = case reads (T.unpack t) of -- `reads` is for String, so T.unpack is needed.
    [(n, "")] -> Just n -- Successfully parsed an Int, and no leftover characters.
    _         -> Nothing

-- mainLoop recursively processes a list of tokens, updating the ForthState.
-- It returns either an error or a tuple containing the new state and any unconsumed tokens.
mainLoop :: [Text] -> ForthState -> Either ForthError (ForthState, [Text])
mainLoop [] state = Right (state, []) -- Base case: no more tokens.
mainLoop (token : restTokens) state
  | T.toLower token == ":" = -- Start of a new word definition.
      -- parseAndStoreDefinition consumes tokens for the definition name and body.
      parseAndStoreDefinition restTokens state >>= \(newState, tokensAfterDef) ->
      mainLoop tokensAfterDef newState -- Continue processing with tokens after the definition.
  | otherwise = -- Not a definition marker, so it's a number or a word to execute.
      evaluateSingleToken token state >>= \newState ->
      mainLoop restTokens newState -- Continue with the next token in the input.

-- Parses and stores a new word definition.
-- Input `definitionTokens` are tokens following ":".
-- Returns the new state and the list of tokens remaining after ";".
parseAndStoreDefinition :: [Text] -> ForthState -> Either ForthError (ForthState, [Text])
parseAndStoreDefinition [] _ = Left InvalidWord -- Missing word name and body after ":".
parseAndStoreDefinition (nameToken : tokensForBody) state@(ForthStateInternal _ defs)
  -- Validate the word name.
  | parseInteger nameToken /= Nothing = Left InvalidWord -- Word name cannot be a number.
  | T.null nameToken = Left InvalidWord -- Word name cannot be empty (T.words should prevent this).
  | T.toLower nameToken == ":" || T.toLower nameToken == ";" = Left InvalidWord -- Cannot redefine ":" or ";".
  | otherwise =
      -- Find the end of the definition body, marked by ";".
      let (rawDefBody, remainingAfterBody) = break ((== ";") . T.toLower) tokensForBody
      in if null remainingAfterBody || T.toLower (head remainingAfterBody) /= ";"
         then Left InvalidWord -- Definition not properly terminated by ";".
         else
           let newWordName = T.toLower nameToken
               -- Store definition tokens in lowercase for case-insensitive execution later.
               processedDefBody = map T.toLower rawDefBody
               newDefinitions = Map.insert newWordName processedDefBody defs
               newState = state { fsDefinitions = newDefinitions }
               tokensAfterSemicolon = tail remainingAfterBody -- Consume the ";".
           in Right (newState, tokensAfterSemicolon)

-- Evaluates a single token. The token can be a number, a built-in operation, or a user-defined word.
-- The `token` parameter is the original (case-preserved) token from input.
evaluateSingleToken :: Text -> ForthState -> Either ForthError ForthState
evaluateSingleToken token state@(ForthStateInternal stack defs)
  | Just num <- parseInteger token = -- If the token is an integer, push it onto the stack.
      Right $ ForthStateInternal (num : stack) defs
  | otherwise = -- The token is a word. Convert to lowercase for case-insensitive matching.
      let lowerWord = T.toLower token
      in case lowerWord of
        -- Built-in arithmetic operations
        "+"  -> opApplyBinary (+) state
        "-"  -> opApplyBinary (flip (-)) state -- For stack (b:a:xs), computes b-a
        "*"  -> opApplyBinary (*) state
        "/"  -> opDivide state
        -- Built-in stack manipulation operations
        "dup"  -> opDup state
        "drop" -> opDrop state
        "swap" -> opSwap state
        "over" -> opOver state
        -- If not a built-in, try to find it in user-defined words.
        customWord ->
          case Map.lookup customWord defs of
            Just definitionTokens ->
              -- Execute the sequence of tokens in the found definition.
              -- `foldM` processes these tokens one by one, updating the state.
              -- `evaluateSingleToken` is recursively called for each token in the definition.
              -- (definitionTokens are already stored in lowercase).
              foldM (flip evaluateSingleToken) state definitionTokens
            Nothing -> Left (UnknownWord token) -- Word is not defined, error with original token.

-- Helper functions for stack operations. Assume stack head is top.

opApplyBinary :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
opApplyBinary op (ForthStateInternal (a:b:xs) defs) = Right $ ForthStateInternal (op b a : xs) defs
opApplyBinary _ (ForthStateInternal _ _) = Left StackUnderflow -- Handles less than 2 elements

opDivide :: ForthState -> Either ForthError ForthState
opDivide (ForthStateInternal (a:b:xs) defs)
  | a == 0    = Left DivisionByZero
  | otherwise = Right $ ForthStateInternal (b `div` a : xs) defs
opDivide (ForthStateInternal _ _) = Left StackUnderflow -- Handles less than 2 elements

opDup :: ForthState -> Either ForthError ForthState
opDup (ForthStateInternal [] _) = Left StackUnderflow
opDup (ForthStateInternal s@(x:_) defs) = Right $ ForthStateInternal (x:s) defs

opDrop :: ForthState -> Either ForthError ForthState
opDrop (ForthStateInternal [] _) = Left StackUnderflow
opDrop (ForthStateInternal (_:xs) defs) = Right $ ForthStateInternal xs defs

opSwap :: ForthState -> Either ForthError ForthState
opSwap (ForthStateInternal (y:x:xs) defs) = Right $ ForthStateInternal (x:y:xs) defs
opSwap (ForthStateInternal _ _) = Left StackUnderflow -- Handles less than 2 elements

opOver :: ForthState -> Either ForthError ForthState
opOver (ForthStateInternal (y:x:xs) defs) = Right $ ForthStateInternal (x:y:x:xs) defs -- (top:second:rest) -> (second:top:second:rest)
opOver (ForthStateInternal _ _) = Left StackUnderflow -- Handles less than 2 elements
