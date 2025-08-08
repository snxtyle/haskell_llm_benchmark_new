{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Control.Monad (foldM)
import Data.Char (isDigit)
import Data.List (break)
import Data.Text (Text)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- Internal instruction set after compilation/expansion
data Instr
  = Push Int
  | AddI
  | SubI
  | MulI
  | DivI
  | DupI
  | DropI
  | SwapI
  | OverI
  deriving (Eq, Show)

type Dict = M.Map Text [Instr]

data ForthState = ForthState
  { stStack :: [Int]    -- top of stack at head
  , stDict  :: Dict
  } deriving (Eq, Show)

emptyState :: ForthState
emptyState = ForthState [] M.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text st0 = evalTokens (tokenize text) st0

toList :: ForthState -> [Int]
toList (ForthState s _) = reverse s

-- Tokenization: words are case-insensitive
tokenize :: Text -> [Text]
tokenize = T.words . T.toLower

-- Main evaluator over tokens
evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] st = Right st
evalTokens (t:ts) (ForthState s d)
  | t == ":" = do
      (rest, d') <- parseDefinition ts d
      evalTokens rest (ForthState s d')
  | otherwise =
      case applyToken d t s of
        Left err -> Left err
        Right s' -> evalTokens ts (ForthState s' d)

-- Parse a definition starting after ":".
-- Returns remaining tokens (after ";") and updated dictionary.
parseDefinition :: [Text] -> Dict -> Either ForthError ([Text], Dict)
parseDefinition [] _ = Left InvalidWord
parseDefinition (name:rest) d
  | isNumberTok name = Left InvalidWord
  | otherwise =
      let (bodyToks, after) = break (== ";") rest
      in case after of
           [] -> Left InvalidWord
           (_semicolon:remaining) -> do
             compiled <- compileTokens d bodyToks
             let d' = M.insert name compiled d
             Right (remaining, d')

-- Compile tokens into instructions using current dictionary.
-- Definitions are expanded at definition time.
compileTokens :: Dict -> [Text] -> Either ForthError [Instr]
compileTokens d = fmap concat . mapM (compileOne d)

compileOne :: Dict -> Text -> Either ForthError [Instr]
compileOne d tok
  | isNumberTok tok = Right [Push (textToInt tok)]
  | Just body  <- M.lookup tok d = Right body
  | Just instr <- builtin tok = Right [instr]
  | otherwise = Left (UnknownWord tok)

-- Apply a single token in execution mode (not during definition)
applyToken :: Dict -> Text -> [Int] -> Either ForthError [Int]
applyToken d tok s
  | isNumberTok tok = Right (textToInt tok : s)
  | Just body <- M.lookup tok d = exec body s
  | Just instr <- builtin tok = applyInstr instr s
  | otherwise = Left (UnknownWord tok)

-- Execute a sequence of instructions on a stack
exec :: [Instr] -> [Int] -> Either ForthError [Int]
exec instrs s0 = foldM (flip exec1Either) s0 instrs

exec1Either :: Instr -> [Int] -> Either ForthError [Int]
exec1Either instr s =
  case applyInstr instr s of
    Left e  -> Left e
    Right s' -> Right s'

applyInstr :: Instr -> [Int] -> Either ForthError [Int]
applyInstr (Push n) s = Right (n : s)
applyInstr AddI s = binOp (+) s
applyInstr SubI s = binOp (-) s
applyInstr MulI s = binOp (*) s
applyInstr DivI s = do
  (x, y, rest) <- pop2 s
  if x == 0
    then Left DivisionByZero
    else Right ((y `quot` x) : rest)
applyInstr DupI s = case s of
  (x:rest) -> Right (x:x:rest)
  _        -> Left StackUnderflow
applyInstr DropI s = case s of
  (_:rest) -> Right rest
  _        -> Left StackUnderflow
applyInstr SwapI s = case s of
  (x:y:rest) -> Right (y:x:rest)
  _          -> Left StackUnderflow
applyInstr OverI s = case s of
  (x:y:rest) -> Right (y:x:y:rest)
  _          -> Left StackUnderflow

binOp :: (Int -> Int -> Int) -> [Int] -> Either ForthError [Int]
binOp f s = do
  (x, y, rest) <- pop2 s
  Right (f y x : rest)

pop2 :: [Int] -> Either ForthError (Int, Int, [Int])
pop2 (x:y:rest) = Right (x, y, rest)
pop2 _          = Left StackUnderflow

builtin :: Text -> Maybe Instr
builtin t = case t of
  "+"    -> Just AddI
  "-"    -> Just SubI
  "*"    -> Just MulI
  "/"    -> Just DivI
  "dup"  -> Just DupI
  "drop" -> Just DropI
  "swap" -> Just SwapI
  "over" -> Just OverI
  _      -> Nothing

isNumberTok :: Text -> Bool
isNumberTok txt = not (T.null txt) && T.all isDigit txt

textToInt :: Text -> Int
textToInt = read . T.unpack
