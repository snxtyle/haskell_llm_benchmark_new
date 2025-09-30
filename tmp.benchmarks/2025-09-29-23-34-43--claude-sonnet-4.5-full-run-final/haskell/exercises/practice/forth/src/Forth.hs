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
import Data.Char (isSpace, toUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Text.Read (readMaybe)

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , definitions :: Map Text [Text]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState { stack = [], definitions = Map.empty }

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = 
  let tokens = tokenize text
  in evalTokens tokens state

tokenize :: Text -> [Text]
tokenize = filter (not . T.null) . T.split isSpace . T.toUpper

evalTokens :: [Text] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens (t:ts) state
  | t == ":" = case parseDefinition ts of
      Left err -> Left err
      Right (name, def, rest) -> evalTokens rest (addDefinition name def state)
  | otherwise = do
      state' <- evalToken t state
      evalTokens ts state'

parseDefinition :: [Text] -> Either ForthError (Text, [Text], [Text])
parseDefinition [] = Left InvalidWord
parseDefinition (name:rest)
  | isNumber name = Left InvalidWord
  | otherwise = 
      let (def, remaining) = span (/= ";") rest
      in case remaining of
           [] -> Left InvalidWord
           (";":rest') -> Right (name, def, rest')
           _ -> Left InvalidWord

isNumber :: Text -> Bool
isNumber t = case T.uncons t of
  Nothing -> False
  Just (c, rest) -> 
    if c == '-'
    then not (T.null rest) && T.all (\ch -> ch >= '0' && ch <= '9') rest
    else T.all (\ch -> ch >= '0' && ch <= '9') t

addDefinition :: Text -> [Text] -> ForthState -> ForthState
addDefinition name def state = 
  let expandedDef = expandDefinition def (definitions state)
  in state { definitions = Map.insert name expandedDef (definitions state) }

expandDefinition :: [Text] -> Map Text [Text] -> [Text]
expandDefinition [] _ = []
expandDefinition (t:ts) defs = 
  case Map.lookup t defs of
    Just def -> def ++ expandDefinition ts defs
    Nothing -> t : expandDefinition ts defs

evalToken :: Text -> ForthState -> Either ForthError ForthState
evalToken token state
  | Just n <- readMaybe (T.unpack token) :: Maybe Int = 
      Right $ state { stack = n : stack state }
  | token == "+" = binaryOp (+) state
  | token == "-" = binaryOp (-) state
  | token == "*" = binaryOp (*) state
  | token == "/" = divOp state
  | token == "DUP" = dupOp state
  | token == "DROP" = dropOp state
  | token == "SWAP" = swapOp state
  | token == "OVER" = overOp state
  | otherwise = case Map.lookup token (definitions state) of
      Just def -> evalTokens def state
      Nothing -> Left (UnknownWord token)

binaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
binaryOp op state = case stack state of
  (a:b:rest) -> Right $ state { stack = (b `op` a) : rest }
  _ -> Left StackUnderflow

divOp :: ForthState -> Either ForthError ForthState
divOp state = case stack state of
  (0:_) -> Left DivisionByZero
  (a:b:rest) -> Right $ state { stack = (b `div` a) : rest }
  _ -> Left StackUnderflow

dupOp :: ForthState -> Either ForthError ForthState
dupOp state = case stack state of
  (a:_) -> Right $ state { stack = a : stack state }
  _ -> Left StackUnderflow

dropOp :: ForthState -> Either ForthError ForthState
dropOp state = case stack state of
  (_:rest) -> Right $ state { stack = rest }
  _ -> Left StackUnderflow

swapOp :: ForthState -> Either ForthError ForthState
swapOp state = case stack state of
  (a:b:rest) -> Right $ state { stack = b : a : rest }
  _ -> Left StackUnderflow

overOp :: ForthState -> Either ForthError ForthState
overOp state = case stack state of
  (a:b:rest) -> Right $ state { stack = b : a : b : rest }
  _ -> Left StackUnderflow

toList :: ForthState -> [Int]
toList state = stack state
