module Brackets (arePaired) where

-- Helper function to check if a character is an opening bracket
isOpen :: Char -> Bool
isOpen c = c `elem` "([{"

-- Helper function to check if a character is a closing bracket
isClose :: Char -> Bool
isClose c = c `elem` ")]}"

-- Helper function to check if an opening bracket matches a closing bracket
matches :: Char -> Char -> Bool
matches '(' ')' = True
matches '[' ']' = True
matches '{' '}' = True
matches _   _   = False -- For any other combination, they don't match

-- Main function to check bracket pairing
arePaired :: String -> Bool
arePaired xs = null (checkBrackets xs [])
  where
    -- checkBrackets :: String -> [Char] -> [Char]
    -- The accumulator `acc` acts as the stack of open brackets.
    -- It returns the final state of the stack.
    -- If an error (mismatch or unmatched closing) occurs, it returns a non-empty list
    -- (e.g., ['_']) to signal failure, which `null` will then interpret as False.
    checkBrackets [] acc = acc -- If the input string is exhausted, return the remaining stack.
    checkBrackets (c:cs) acc
      | isOpen c = checkBrackets cs (c:acc) -- If it's an opening bracket, push it onto the stack.
      | isClose c =
          case acc of
            (top:rest) -> -- If the stack is not empty, pop the top element.
              if matches top c
                then checkBrackets cs rest -- If the popped opening bracket matches the current closing bracket, continue.
                else ['_'] -- Mismatch: return a non-empty list to indicate failure.
            [] -> ['_'] -- Unmatched closing bracket: stack is empty, but a closing bracket was found. Indicate failure.
      | otherwise = checkBrackets cs acc -- Ignore any other characters.
