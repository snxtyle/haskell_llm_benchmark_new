module Acronym (abbreviate) where

import Data.Char (isAlpha, isSpace, isUpper, isLower, toUpper)

-- Helper function to insert spaces for camelCase and acronym splits.
-- Also handles hyphen replacement and general punctuation removal.
insertSplits :: String -> String
insertSplits [] = []
insertSplits [c]
    | isAlpha c = [c]
    | otherwise = [] -- Remove non-alpha single characters
insertSplits (c1:c2:cs)
    | c1 == '-' = ' ' : insertSplits (c2:cs) -- Hyphen becomes space
    | not (isAlpha c1) && not (isSpace c1) = insertSplits (c2:cs) -- Other punctuation removed
    | isLower c1 && isUpper c2 = c1 : ' ' : insertSplits (c2:cs) -- camelCase split (e.g., "camelCase" -> "camel Case")
    -- This rule handles cases like "HTTPResponse" -> "HTTP Response".
    -- It inserts a space if an uppercase letter is followed by another uppercase letter,
    -- and that second uppercase letter is followed by a lowercase letter.
    | isUpper c1 && isUpper c2 && not (null cs) && isLower (head cs) = c1 : ' ' : insertSplits (c2:cs)
    | otherwise = c1 : insertSplits (c2:cs)

abbreviate :: String -> String
abbreviate xs =
    let
        -- First, check if the entire input (after removing non-alpha chars) is an acronym.
        -- This handles cases like "HTML" -> "HTML" and "PHP" -> "PHP".
        alphaOnly = filter isAlpha xs
    in if not (null alphaOnly) && all isUpper alphaOnly
       then alphaOnly
       else
           let
               -- Apply the splitting logic to the input string
               processedString = insertSplits xs
               -- Split the processed string into words using standard 'words' function
               wordList = words processedString
               -- Take the first letter of each word and convert to uppercase
               -- This applies to "Portable", "Network", "Graphics", "Response", etc.
               firstLetters = [toUpper (head w) | w <- wordList, not (null w)]
           in
               firstLetters
