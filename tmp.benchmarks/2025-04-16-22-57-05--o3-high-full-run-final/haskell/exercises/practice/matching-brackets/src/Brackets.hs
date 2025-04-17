module Brackets (arePaired) where

-- | Determine whether all types of brackets are correctly paired and nested
--   in the given string. Characters that are not brackets are ignored.
arePaired :: String -> Bool
arePaired = go []
  where
    -- The first argument is the stack of currently open brackets that
    -- still need to be closed.  The second argument is the remaining
    -- characters to process.
    go :: [Char] -> String -> Bool
    go []      []       = True         -- no more input, stack empty  -> OK
    go _       []       = False        -- no more input, but stack not empty
    go stack   (c : cs)
      | isOpen  c = go (c : stack) cs  -- push opening bracket
      | isClose c =
          case stack of                -- need a matching opening bracket
            (s : ss) | matches s c -> go ss cs
            _                       -> False
      | otherwise = go stack cs        -- ignore non‑bracket characters

    isOpen, isClose :: Char -> Bool
    isOpen  = (`elem` "([{")
    isClose = (`elem` ")]}")

    matches :: Char -> Char -> Bool
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _   _   = False
