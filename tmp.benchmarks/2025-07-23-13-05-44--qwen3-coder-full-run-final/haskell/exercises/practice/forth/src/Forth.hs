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
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (toLower, isDigit)
import Data.List (foldl')

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState 
  { stack :: [Int]
  , definitions :: Map Text [Token]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState [] Map.empty

data Token 
  = TNum Int
  | TWord Text
  deriving (Show, Eq)

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state = 
  case parseTokens (T.words text) of
    Left err -> Left err
    Right tokens -> evalTokens tokens state

toList :: ForthState -> [Int]
toList (ForthState s _) = reverse s

parseTokens :: [Text] -> Either ForthError [Token]
parseTokens words = parseTokens' words []

parseTokens' :: [Text] -> [Token] -> Either ForthError [Token]
parseTokens' [] acc = Right (reverse acc)
parseTokens' (w:ws) acc = 
  if T.toLower w == ":"
    then parseDefinition ws acc
    else case parseSingleToken w of
      Left err -> Left err
      Right token -> parseTokens' ws (token:acc)

parseSingleToken :: Text -> Either ForthError Token
parseSingleToken t
  | T.all isDigit t || (T.length t > 1 && T.head t == '-' && T.all isDigit (T.tail t)) = 
      Right $ TNum (read $ T.unpack t)
  | otherwise = Right $ TWord (T.toLower t)

parseDefinition :: [Text] -> [Token] -> Either ForthError [Token]
parseDefinition [] _ = Left InvalidWord
parseDefinition (name:rest) acc = 
  case findSemicolon rest [] of
    Nothing -> Left InvalidWord
    Just (bodyWords, remaining) -> 
      let defToken = TWord (T.toLower name)
          bodyTokens = map (either (error "Parse error in definition") id) 
                          (map parseSingleToken bodyWords)
          defTokens = TWord ":" : defToken : bodyTokens ++ [TWord ";"]
      in parseTokens' remaining (defTokens ++ acc)

findSemicolon :: [Text] -> [Text] -> Maybe ([Text], [Text])
findSemicolon [] _ = Nothing
findSemicolon (w:ws) acc =
  if T.toLower w == ";"
    then Just (reverse acc, ws)
    else findSemicolon ws (w:acc)

evalTokens :: [Token] -> ForthState -> Either ForthError ForthState
evalTokens [] state = Right state
evalTokens tokens state = evalTokens' tokens state []

evalTokens' :: [Token] -> ForthState -> [Token] -> Either ForthError ForthState
evalTokens' [] state _ = Right state
evalTokens' (TWord ":":TWord name:tokens) state defStack = 
  case findEndDef tokens [] of
    Nothing -> Left InvalidWord
    Just (body, rest) -> 
      let newState = state { definitions = Map.insert name body (definitions state) }
      in evalTokens' rest newState defStack
evalTokens' (TWord ";":tokens) state (_:defStack) = 
  evalTokens' tokens state defStack
evalTokens' (token:tokens) state defStack = 
  case evalToken token state defStack of
    Left err -> Left err
    Right (newState, newDefStack) -> evalTokens' tokens newState newDefStack

findEndDef :: [Token] -> [Token] -> Maybe ([Token], [Token])
findEndDef [] _ = Nothing
findEndDef (TWord ";":rest) acc = Just (reverse acc, rest)
findEndDef (token:tokens) acc = findEndDef tokens (token:acc)

evalToken :: Token -> ForthState -> [Token] -> Either ForthError (ForthState, [Token])
evalToken (TNum n) state defStack = Right (state { stack = n : stack state }, defStack)
evalToken (TWord w) state defStack = 
  case w of
    ":" -> Right (state, TWord w:defStack)
    _ -> case Map.lookup w (definitions state) of
      Just def -> do
        result <- evalTokens def state
        Right (result, defStack)
      Nothing -> case w of
        "+" -> doOp (+) state defStack
        "-" -> doOp (-) state defStack
        "*" -> doOp (*) state defStack
        "/" -> doDiv state defStack
        "dup" -> doDup state defStack
        "drop" -> doDrop state defStack
        "swap" -> doSwap state defStack
        "over" -> doOver state defStack
        _ -> Left $ UnknownWord w

doOp :: (Int -> Int -> Int) -> ForthState -> [Token] -> Either ForthError (ForthState, [Token])
doOp op state defStack = 
  case stack state of
    (a:b:rest) -> Right (state { stack = (b `op` a) : rest }, defStack)
    _ -> Left StackUnderflow

doDiv :: ForthState -> [Token] -> Either ForthError (ForthState, [Token])
doDiv state defStack = 
  case stack state of
    (0:_) -> Left DivisionByZero
    (a:b:rest) -> Right (state { stack = (b `div` a) : rest }, defStack)
    _ -> Left StackUnderflow

doDup :: ForthState -> [Token] -> Either ForthError (ForthState, [Token])
doDup state defStack = 
  case stack state of
    (x:xs) -> Right (state { stack = x:x:xs }, defStack)
    [] -> Left StackUnderflow

doDrop :: ForthState -> [Token] -> Either ForthError (ForthState, [Token])
doDrop state defStack = 
  case stack state of
    (_:xs) -> Right (state { stack = xs }, defStack)
    [] -> Left StackUnderflow

doSwap :: ForthState -> [Token] -> Either ForthError (ForthState, [Token])
doSwap state defStack = 
  case stack state of
    (a:b:xs) -> Right (state { stack = b:a:xs }, defStack)
    _ -> Left StackUnderflow

doOver :: ForthState -> [Token] -> Either ForthError (ForthState, [Token])
doOver state defStack = 
  case stack state of
    (a:b:xs) -> Right (state { stack = b:a:b:xs }, defStack)
    _ -> Left StackUnderflow
