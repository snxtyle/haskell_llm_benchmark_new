{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , toList
  , emptyState
  ) where

import           Data.Char          (isDigit, toLower)
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import qualified Data.Text          as T

------------------------------------------------------------
-- Public API
------------------------------------------------------------

data ForthError
     = DivisionByZero
     | StackUnderflow
     | InvalidWord
     | UnknownWord Text
     deriving (Show, Eq)

-- | A forth state is a stack plus the current dictionary
data ForthState =
  FS { fsStack :: [Int]                -- ^ top of stack is the *head*
     , fsDict  :: Map Text [Token]     -- ^ user‑defined words
     }
  deriving (Show, Eq)

-- | An entirely empty state
emptyState :: ForthState
emptyState = FS [] Map.empty

-- | Evaluate some forth source code given the current state.
evalText :: Text -> ForthState -> Either ForthError ForthState
evalText src st =
  run (tokenise src) st

-- | Return the stack as a list, element on top must be the *last* element.
toList :: ForthState -> [Int]
toList = reverse . fsStack

------------------------------------------------------------
-- Internal machinery
------------------------------------------------------------

-----------------------------------------------------------------
-- Tokens
-----------------------------------------------------------------
-- We first break the input text into simple lexical tokens,
-- already lower‑casing everything because the language is
-- case‑insensitive.
data Token = TNum Int | TWord Text
  deriving (Show, Eq)

toLowerText :: Text -> Text
toLowerText = T.toLower

isNumberTxt :: Text -> Bool
isNumberTxt t =
  let s = T.unpack t
  in case s of
       ('-':ds) -> not (null ds) && all isDigit ds
       _        -> all isDigit s

parseToken :: Text -> Token
parseToken t
  | isNumberTxt t = TNum (read (T.unpack t))
  | otherwise     = TWord (toLowerText t)

tokenise :: Text -> [Token]
tokenise = map parseToken . T.words

-----------------------------------------------------------------
-- Core evaluator
-----------------------------------------------------------------
type Eval a = Either ForthError a

run :: [Token] -> ForthState -> Eval ForthState
run [] st = Right st
run (tok:rest) st = case tok of
  TNum n      -> run rest (push n st)
  TWord ":"   -> defineWord rest st
  TWord w     -> executeWord w rest st

-----------------------------------------------------------------
-- Stack helpers
-----------------------------------------------------------------
push :: Int -> ForthState -> ForthState
push n st = st { fsStack = n : fsStack st }

pop :: ForthState -> Eval (Int, ForthState)
pop st = case fsStack st of
           []     -> Left StackUnderflow
           (x:xs) -> Right (x, st { fsStack = xs })

need :: Int -> ForthState -> Eval ()
need n st | length (fsStack st) < n = Left StackUnderflow
          | otherwise               = Right ()

-----------------------------------------------------------------
-- Built‑in words
-----------------------------------------------------------------
executeBuiltin :: Text -> ForthState -> Eval ForthState
executeBuiltin w st = case w of
  "+"   -> binOp (+)
  "-"   -> binOp (-)
  "*"   -> binOp (*)
  "/"   -> divOp
  "dup" -> do
             need 1 st
             let (x:xs) = fsStack st
             Right st { fsStack = x:x:xs }
  "drop"-> do
             need 1 st
             let (_:xs) = fsStack st
             Right st { fsStack = xs }
  "swap"-> do
             need 2 st
             let (a:b:xs) = fsStack st
             Right st { fsStack = b:a:xs }
  "over"-> do
             need 2 st
             let (a:b:xs) = fsStack st
             Right st { fsStack = b:a:b:xs }
  _     -> Left (UnknownWord w)
 where
   binOp f = do
      need 2 st
      let (a:b:xs) = fsStack st
      Right st { fsStack = f b a : xs }
   divOp = do
      need 2 st
      let (a:b:xs) = fsStack st
      if a == 0
         then Left DivisionByZero
         else Right st { fsStack = b `div` a : xs }

-----------------------------------------------------------------
-- Word execution
-----------------------------------------------------------------
executeWord :: Text -> [Token] -> ForthState -> Eval ForthState
executeWord w rest st =
  case Map.lookup w (fsDict st) of
    Just body -> run (body ++ rest) st           -- user‑defined
    Nothing   -> do                              -- maybe builtin
                  st' <- executeBuiltin w st
                  run rest st'

-----------------------------------------------------------------
-- Word definition
-----------------------------------------------------------------
defineWord :: [Token] -> ForthState -> Eval ForthState
defineWord ts st =
  case ts of
    (TNum _ : _) -> Left InvalidWord                -- numbers invalid names
    (TWord nameLower : defBody) ->
        let name = toLowerText nameLower
            (bodyTokens, restAfterSemi) = break isSemi defBody
        in case restAfterSemi of
             [] -> Left InvalidWord                 -- missing ';'
             (_semiTok:remaining) ->
               let compiledBody = map compile bodyTokens
                   newDict      = Map.insert name compiledBody (fsDict st)
               in run remaining (st { fsDict = newDict })
    _ -> Left InvalidWord                           -- no name following ':'
  where
    isSemi (TWord ";") = True
    isSemi _           = False

    compile t@(TNum _) = t
    compile (TWord w)  = TWord (toLowerText w)

