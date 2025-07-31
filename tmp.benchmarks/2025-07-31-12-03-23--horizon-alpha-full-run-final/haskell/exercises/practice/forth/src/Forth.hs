{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Char (isDigit)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- The Forth state consists of a stack and a dictionary of user-defined words.
data ForthState = ForthState
  { fsStack :: [Int]                -- Top of stack is head of list
  , fsDict  :: Map Text [Token]     -- Dictionary maps word name (lowercased) to its token sequence
  } deriving (Show, Eq)

data Token
  = TNum Int
  | TWord Text
  deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState { fsStack = [], fsDict = M.empty }

-- Public API: evaluate a chunk of text starting from a state.
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text st0 = do
  -- Tokenize into a sequence that may include definitions; expand definitions into dict and run non-definition tokens.
  tokensOrDefs <- tokenize text
  execTopLevel tokensOrDefs st0

toList :: ForthState -> [Int]
toList = reverse . fsStack

-- Parsing/tokenization

-- Tokenize input text into either top-level tokens or definitions.
-- Result is a list of Top items: either a definition (Def name tokens) that updates dict,
-- or a token to execute at top level.
data Top
  = TopTok Token
  | TopDef Text [Token]
  deriving (Show, Eq)

tokenize :: Text -> Either ForthError [Top]
tokenize t = goTop (lexWords t)
  where
    -- split by whitespace, preserve raw items
    lexWords :: Text -> [Text]
    lexWords = filter (not . T.null) . T.words

    goTop :: [Text] -> Either ForthError [Top]
    goTop [] = Right []
    goTop (w:ws)
      | T.toLower w == ":" = do
          (name, bodyToks, rest) <- parseDef ws
          more <- goTop rest
          pure (TopDef name bodyToks : more)
      | otherwise = do
          tok <- parseToken w
          more <- goTop ws
          pure (TopTok tok : more)

    parseDef :: [Text] -> Either ForthError (Text, [Token], [Text])
    parseDef [] = Left InvalidWord
    parseDef (n:ws) = do
      -- word name cannot be a number
      case parseNumber n of
        Just _  -> Left InvalidWord
        Nothing -> do
          let name = T.toLower n
          (body, rest) <- collectUntilSemicolon [] ws
          pure (name, body, rest)

    collectUntilSemicolon :: [Token] -> [Text] -> Either ForthError ([Token], [Text])
    collectUntilSemicolon _ [] = Left InvalidWord
    collectUntilSemicolon acc (w:ws)
      | T.toLower w == ";" = Right (reverse acc, ws)
      | otherwise = do
          tok <- parseToken w
          collectUntilSemicolon (tok:acc) ws

    parseToken :: Text -> Either ForthError Token
    parseToken w =
      case parseNumber w of
        Just n  -> Right (TNum n)
        Nothing -> Right (TWord (T.toLower w))

    parseNumber :: Text -> Maybe Int
    parseNumber txt =
      let s = T.unpack txt
      in case s of
           ('-':ds) | all isDigit ds, not (null ds) -> Just (read s)
           _ | all isDigit s, not (null s) -> Just (read s)
           _ -> Nothing

-- Execute a list of Top items (definitions update dict; tokens execute with current dict)
execTopLevel :: [Top] -> ForthState -> Either ForthError ForthState
execTopLevel [] st = Right st
execTopLevel (x:xs) st = case x of
  TopDef name toks -> do
    -- Expand any references to the word itself is allowed (typical Forth allows recursion); no special check needed.
    let st' = st { fsDict = M.insert name toks (fsDict st) }
    execTopLevel xs st'
  TopTok tok -> do
    st' <- execToken tok st
    execTopLevel xs st'

-- Execute a single token
execToken :: Token -> ForthState -> Either ForthError ForthState
execToken (TNum n) st = Right st { fsStack = n : fsStack st }
execToken (TWord w) st =
  case M.lookup w (fsDict st) of
    Just body -> execTokens body st
    Nothing   -> execBuiltin w st

-- Execute a sequence of tokens
execTokens :: [Token] -> ForthState -> Either ForthError ForthState
execTokens [] st = Right st
execTokens (t:ts) st = do
  st' <- execToken t st
  execTokens ts st'

-- Built-in words
execBuiltin :: Text -> ForthState -> Either ForthError ForthState
execBuiltin w st =
  case w of
    "+"   -> binOp (+) st
    "-"   -> binOp (-) st
    "*"   -> binOp (*) st
    "/"   -> divOp st
    "dup" -> dupOp st
    "drop"-> dropOp st
    "swap"-> swapOp st
    "over"-> overOp st
    -- Unknown word
    _     -> Left (UnknownWord w)

-- Helpers for stack ops
pop :: ForthState -> Either ForthError (Int, ForthState)
pop st = case fsStack st of
  (x:xs) -> Right (x, st { fsStack = xs })
  []     -> Left StackUnderflow

push :: Int -> ForthState -> ForthState
push x st = st { fsStack = x : fsStack st }

binOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binOp f st = do
  (a, st1) <- pop st
  (b, st2) <- pop st1
  let r = f b a
  pure (push r st2)

divOp :: ForthState -> Either ForthError ForthState
divOp st = do
  (a, st1) <- pop st
  (b, st2) <- pop st1
  if a == 0
    then Left DivisionByZero
    else pure (push (b `quot` a) st2)

dupOp :: ForthState -> Either ForthError ForthState
dupOp st = do
  (a, st1) <- pop st
  pure (push a (push a st1))

dropOp :: ForthState -> Either ForthError ForthState
dropOp st = do
  (_, st1) <- pop st
  pure st1

swapOp :: ForthState -> Either ForthError ForthState
swapOp st = do
  (a, st1) <- pop st
  (b, st2) <- pop st1
  pure (push a (push b st2))

overOp :: ForthState -> Either ForthError ForthState
overOp st = do
  (a, st1) <- pop st
  (b, st2) <- pop st1
  pure (push b (push a (push b st2)))
