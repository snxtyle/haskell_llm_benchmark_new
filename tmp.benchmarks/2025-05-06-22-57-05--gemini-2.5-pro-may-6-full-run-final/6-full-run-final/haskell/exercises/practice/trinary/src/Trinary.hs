module Trinary (readTri, showTri) where

-- No explicit imports are needed as all functions used (mapM, foldl, seq, fromIntegral, etc.)
-- are available in the Prelude or can be constructed from Prelude functions.
-- String, Integer, Int, Char, Maybe, case, lambda expressions are all part of standard Haskell.

readTri :: String -> Integer
readTri s =
  -- Attempt to parse the string into a list of trinary digits (0, 1, or 2).
  -- If parsing fails (e.g., due to an invalid character), parseTrits returns Nothing.
  case parseTrits s of
    Nothing    -> 0 -- As per instructions, invalid trinary strings convert to 0.
    Just trits -> calculateDecimalValue trits -- If parsing succeeds, convert the list of digits.
  where
    -- Helper function to convert a single character to its trinary integer value.
    -- Returns Nothing if the character is not a valid trinary digit ('0', '1', '2').
    charToTrinaryDigit :: Char -> Maybe Int
    charToTrinaryDigit '0' = Just 0
    charToTrinaryDigit '1' = Just 1
    charToTrinaryDigit '2' = Just 2
    charToTrinaryDigit _   = Nothing -- Any other character is invalid.

    -- Helper function to parse the input string into a list of integer trinary digits.
    -- It uses `mapM` which applies `charToTrinaryDigit` to each character.
    -- If all characters are valid, it returns `Just [digits]`.
    -- If any character is invalid, `mapM` short-circuits and returns `Nothing`.
    -- `mapM` is equivalent to `sequence . map`, both `sequence` and `map` are in Prelude.
    parseTrits :: String -> Maybe [Int]
    parseTrits str = mapM charToTrinaryDigit str

    -- Helper function to calculate the decimal value from a list of trinary digits.
    -- Example: [1, 0, 2] (for "102") -> ( (1*3 + 0)*3 + 2 ) = 11.
    -- The result type is Integer to handle potentially large numbers.
    calculateDecimalValue :: [Int] -> Integer
    calculateDecimalValue [] = 0 -- An empty list of trits (e.g., from an empty input string) results in 0.
    calculateDecimalValue trits =
      strictFoldl (\accumulator digit -> accumulator * 3 + fromIntegral digit) 0 trits
      -- The initial accumulator is (0 :: Integer).
      -- `fromIntegral digit` converts the Int digit to Integer for the calculation.

    -- A strict left fold, implemented using `seq` from Prelude.
    -- This is to ensure efficiency and avoid potential space leaks when the
    -- combining function is strict (like arithmetic operations on Integers).
    -- `acc` is the accumulator, `x` is the current element from the list `xs`.
    -- The type `b` will be Integer and `a` will be Int in this usage.
    strictFoldl :: (b -> a -> b) -> b -> [a] -> b
    strictFoldl _ acc [] = acc -- Base case: empty list returns the accumulator.
    strictFoldl f acc (x:xs) =
      let newAcc = f acc x -- Apply the function to the current accumulator and element.
      in newAcc `seq` strictFoldl f newAcc xs -- Ensure newAcc is evaluated before the recursive call.

-- The showTri function is not implemented as per the current request,
-- but its type signature is provided to satisfy the compiler and test suite.
showTri :: Integer -> String
showTri = error "You need to implement this function."
