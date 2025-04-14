module Brackets (arePaired) where

import Data.List (foldl')

arePaired :: String -> Bool
arePaired xs = arePairedHelper (filter isBracket xs)
  where
    isBracket c = c `elem` "(){}[]"

    arePairedHelper :: String -> Bool
    arePairedHelper str = null $ foldl' pushOrPop [] str
      where
        pushOrPop stack c
          | c `elem` "([{" = c : stack  -- Push opening bracket
          | otherwise = -- c is a closing bracket
              case c of
                ')' -> if not (null stack) && head stack == '(' then tail stack else c : stack
                ']' -> if not (null stack) && head stack == '[' then tail stack else c : stack
                '}' -> if not (null stack) && head stack == '{' then tail stack else c : stack
                _   -> stack  -- Should not happen, but return stack as is
