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
import qualified Data.Char as C

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

data ForthState = ForthState
  { stack :: [Int]
  , definitions :: [(Text, [Text])]
  } deriving (Show, Eq)

emptyState :: ForthState
emptyState = ForthState { stack = [], definitions = [] }

evalText :: Text -> ForthState -> Either ForthError ForthState
evalText text state =
  let ws = map T.toLower (T.words text)
  in evalAll ws state

toList :: ForthState -> [Int]
toList = reverse . stack

-- Main evaluation loop that processes a list of words.
evalAll :: [Text] -> ForthState -> Either ForthError ForthState
evalAll [] state = Right state
evalAll (w:ws) state
  | w == ":"  = defineWord ws state
  | otherwise = evalWord w state >>= evalAll ws

-- Handles word definition syntax: ': word ... ;'
defineWord :: [Text] -> ForthState -> Either ForthError ForthState
defineWord [] _ = Left InvalidWord -- Missing name, definition and semicolon.
defineWord (name:rest) state
  | isNumber name = Left InvalidWord
  | otherwise =
      let (def, remaining) = span (/= ";") rest
      in if null remaining
         then Left InvalidWord -- Missing semicolon.
         else
           let newDefs = (name, def) : definitions state
               newState = state { definitions = newDefs }
           in evalAll (tail remaining) newState

-- Evaluates a single word.
evalWord :: Text -> ForthState -> Either ForthError ForthState
evalWord w state
  | isNumber w = Right $ state { stack = (read . T.unpack $ w) : stack state }
  | Just def <- lookup w (definitions state) = evalAll def state
  | otherwise = case w of
      "+"    -> applyBinaryOp (+) state
      "-"    -> applyBinaryOp (-) state
      "*"    -> applyBinaryOp (*) state
      "/"    -> applyDiv state
      "dup"  -> opDup state
      "drop" -> opDrop state
      "swap" -> opSwap state
      "over" -> opOver state
      _      -> Left (UnknownWord w)

isNumber :: Text -> Bool
isNumber t = not (T.null t) && T.all C.isDigit t

-- Stack operation helpers

applyBinaryOp :: (Int -> Int -> Int) -> ForthState -> Either ForthError ForthState
applyBinaryOp op (ForthState (y:x:xs) defs) = Right $ ForthState ((x `op` y) : xs) defs
applyBinaryOp _ _ = Left StackUnderflow

applyDiv :: ForthState -> Either ForthError ForthState
applyDiv (ForthState (0:_:_) _) = Left DivisionByZero
applyDiv (ForthState (y:x:xs) defs) = Right $ ForthState ((x `div` y) : xs) defs
applyDiv _ = Left StackUnderflow

opDup :: ForthState -> Either ForthError ForthState
opDup (ForthState (x:xs) defs) = Right $ ForthState (x:x:xs) defs
opDup _ = Left StackUnderflow

opDrop :: ForthState -> Either ForthError ForthState
opDrop (ForthState (_:xs) defs) = Right $ ForthState xs defs
opDrop _ = Left StackUnderflow

opSwap :: ForthState -> Either ForthError ForthState
opSwap (ForthState (y:x:xs) defs) = Right $ ForthState (x:y:xs) defs
opSwap _ = Left StackUnderflow

opOver :: ForthState -> Either ForthError ForthState
opOver (ForthState (y:x:xs) defs) = Right $ ForthState (x:y:x:xs) defs
opOver _ = Left StackUnderflow
