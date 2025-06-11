module Brackets (arePaired) where

-- | Verify that all brackets, braces and parentheses in the input
--   string are correctly matched and nested.
--   Non-bracket characters are ignored.
arePaired :: String -> Bool
arePaired = go []
  where
    go :: [Char] -> String -> Bool
    -- When we've processed the entire input, succeed only if no unmatched
    -- opening brackets remain.
    go [] [] = True
    go _  [] = False

    go stack (c:cs)
        -- Opening brackets: push onto the stack.
        | c `elem` "([{" = go (c : stack) cs

        -- Closing brackets: stack must be non-empty and the top must match.
        | c `elem` ")]}" =
            case stack of
                (s:rest) | matches s c -> go rest cs
                _                      -> False

        -- Any other character: ignore it.
        | otherwise = go stack cs

    -- Match an opening bracket with its corresponding closing bracket.
    matches :: Char -> Char -> Bool
    matches '(' ')' = True
    matches '[' ']' = True
    matches '{' '}' = True
    matches _   _   = False
