module Brackets (arePaired) where

-- | Check whether all the brackets in a string are correctly matched and nested.
--
--   Only the following three kinds of brackets are considered:
--     * round brackets:   ( )
--     * square brackets:  [ ]
--     * curly brackets:   { }
--
--   Any other characters in the input are ignored.
--
--   Examples:
--
--   >>> arePaired "{[]}"
--   True
--
--   >>> arePaired "{[}]"
--   False
arePaired :: String -> Bool
arePaired = go []
  where
    go :: [Char] -> String -> Bool
    -- If we've consumed the whole input the stack must be empty
    go []     []     = True
    go _      []     = False  -- leftover unmatched opening brackets
    go stack  (c:cs)
      | isOpening c           = go (c:stack) cs
      | isClosing c           = case stack of
                                  (o:os) | matches o c -> go os cs
                                  _                    -> False
      | otherwise             = go stack cs  -- ignore non‑bracket characters

    -- Helper predicates and matching function
    isOpening :: Char -> Bool
    isOpening c = c `elem` "([{"

    isClosing :: Char -> Bool
    isClosing c = c `elem` ")]}"

    matches :: Char -> Char -> Bool
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _   _   = False
