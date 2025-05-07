module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = go xs []
  where
    -- Helper function to process the string with a stack
    -- `str` is the remaining part of the input string
    -- `stack` is a list of characters representing the open brackets encountered
    go :: String -> String -> Bool
    go [] [] = True  -- End of string, empty stack: perfectly balanced
    go [] _  = False -- End of string, non-empty stack: unclosed brackets
    go (c:cs) stack
      | isOpening c = go cs (c:stack) -- Push opening bracket onto stack
      | isClosing c = case stack of
          []     -> False             -- Closing bracket with no matching open bracket
          (s:ss) -> if matches s c
                      then go cs ss    -- Matching pair found, pop from stack
                      else False       -- Mismatched brackets
      | otherwise   = go cs stack     -- Ignore non-bracket characters

    -- Checks if a character is an opening bracket
    isOpening :: Char -> Bool
    isOpening c = c `elem` "([{"

    -- Checks if a character is a closing bracket
    isClosing :: Char -> Bool
    isClosing c = c `elem` ")]}"

    -- Checks if an opening bracket matches a closing bracket
    matches :: Char -> Char -> Bool
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _   _   = False
