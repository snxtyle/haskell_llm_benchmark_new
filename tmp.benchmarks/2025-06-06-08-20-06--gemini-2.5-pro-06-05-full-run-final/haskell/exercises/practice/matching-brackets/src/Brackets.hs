module Brackets (arePaired) where

arePaired :: String -> Bool
arePaired xs =
  case foldl step (Just []) xs of
    Just [] -> True
    _       -> False
  where
    step :: Maybe [Char] -> Char -> Maybe [Char]
    step Nothing _ = Nothing -- An error occurred, propagate failure
    step (Just stack) c
      | isOpening c = Just (c:stack)
      | isClosing c = pop c stack
      | otherwise   = Just stack -- Ignore other characters

    isOpening :: Char -> Bool
    isOpening c = c `elem` "([{"

    isClosing :: Char -> Bool
    isClosing c = c `elem` ")]}"

    -- Try to pop a character from the stack if it matches the closing bracket
    pop :: Char -> [Char] -> Maybe [Char]
    pop _ [] = Nothing -- Closing bracket with no corresponding opening one
    pop closer (opener:rest)
      | matches closer opener = Just rest
      | otherwise             = Nothing -- Mismatched brackets

    -- Check if a closing bracket matches an opening one
    matches :: Char -> Char -> Bool
    matches ')' '(' = True
    matches ']' '[' = True
    matches '}' '{' = True
    matches _   _   = False
