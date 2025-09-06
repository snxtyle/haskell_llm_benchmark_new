{-# LANGUAGE OverloadedStrings #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import Data.Text (Text, pack, unpack, toUpper, words)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isDigit)
import Control.Monad (foldM)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , definitions :: Map Text [Text]
  }

emptyState :: ForthState
emptyState = ForthState [] Map.empty

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = processTokens (map T.toUpper $ T.words text) state

processTokens :: [Text] -> ForthState -> Either ForthError ForthState
processTokens [] s = Right s
processTokens (t:ts) s = case t of
  ":" -> do
    (name, def, rest) <- parseDefinition ts
    let newDefs = Map.insert name def (definitions s)
    processTokens rest (s { definitions = newDefs })
  _ -> do
    s' <- handleToken t s
    processTokens ts s'

parseDefinition :: [Text] -> Either ForthError (Text, [Text], [Text])
parseDefinition [] = Left InvalidWord
parseDefinition (name:ts) = go [] ts
  where
    go acc (t:ts') | t == ";" = Right (name, reverse acc, ts')
    go acc (t:ts') = go (t:acc) ts'
    go _ [] = Left InvalidWord

handleToken :: Text -> ForthState -> Either ForthError ForthState
handleToken t s
  | isNumber t = Right $ push (readInt t) s
  | Just def <- Map.lookup t (definitions s) = processTokens def s
  | otherwise = case t of
      "+" -> binOp (+) s
      "-" -> binOp (-) s
      "*" -> binOp (*) s
      "/" -> divOp s
      "DUP" -> dupOp s
      "DROP" -> dropOp s
      "SWAP" -> swapOp s
      "OVER" -> overOp s
      _ -> Left $ UnknownWord t

isNumber :: Text -> Bool
isNumber t = T.all isDigit t

readInt :: Text -> Int
readInt t = read (T.unpack t)

push :: Int -> ForthState -> ForthState
push x s = s { stack = x : stack s }

pop :: ForthState -> Either ForthError (Int, ForthState)
pop s = case stack s of
  [] -> Left StackUnderflow
  (x:xs) -> Right (x, s { stack = xs })

binOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binOp op s = do
  (b, s1) <- pop s
  (a, s2) <- pop s1
  Right $ push (op a b) s2

divOp :: ForthState -> Either ForthError ForthState
divOp s = do
  (b, s1) <- pop s
  if b == 0 then Left DivisionByZero else do
    (a, s2) <- pop s1
    Right $ push (a `div` b) s2

dupOp :: ForthState -> Either ForthError ForthState
dupOp s = do
  (x, _) <- pop s
  Right $ push x s

dropOp :: ForthState -> Either ForthError ForthState
dropOp s = fmap snd (pop s)

swapOp :: ForthState -> Either ForthError ForthState
swapOp s = do
  (b, s1) <- pop s
  (a, s2) <- pop s1
  Right $ push b $ push a s2

overOp :: ForthState -> Either ForthError ForthState
overOp s = do
  (b, s1) <- pop s
  (a, _) <- pop s1
  Right $ push a s

toList :: ForthState -> [Int]
toList = stack
