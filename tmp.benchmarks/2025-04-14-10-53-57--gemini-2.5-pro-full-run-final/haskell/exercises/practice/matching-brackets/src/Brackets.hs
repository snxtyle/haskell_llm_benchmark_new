module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs = go xs []
  where
    -- Helper function:
    -- Takes the remaining input string and the current stack of opening brackets.
    -- Returns True if the brackets in the original string are balanced, False otherwise.
    go :: String -> String -> Bool
    go [] [] = True  -- Base case: End of input, empty stack -> balanced
    go [] _  = False -- Base case: End of input, non-empty stack -> unbalanced (unclosed openers)
    go (c:cs) stack =
      case c of
        -- Opening brackets: push onto the stack
        '[' -> go cs ('[':stack)
        '{' -> go cs ('{':stack)
        '(' -> go cs ('(':stack)

        -- Closing brackets: check against the top of the stack
        ']' -> case stack of
                 ('[':rest) -> go cs rest -- Found matching '[', pop it
                 _          -> False      -- Mismatched or stack empty

        '}' -> case stack of
                 ('{':rest) -> go cs rest -- Found matching '{', pop it
                 _          -> False      -- Mismatched or stack empty

        ')' -> case stack of
                 ('(':rest) -> go cs rest -- Found matching '(', pop it
                 _          -> False      -- Mismatched or stack empty

        -- Other characters: ignore and continue
        _   -> go cs stack
