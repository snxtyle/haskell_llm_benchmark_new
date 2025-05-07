module Say (inEnglish) where

-- Using lookup for number to name conversion
onesMap :: [(Integer, String)]
onesMap = [ (0, "zero"), (1, "one"), (2, "two"), (3, "three"), (4, "four")
          , (5, "five"), (6, "six"), (7, "seven"), (8, "eight"), (9, "nine") ]

teensMap :: [(Integer, String)]
teensMap = [ (10, "ten"), (11, "eleven"), (12, "twelve"), (13, "thirteen")
           , (14, "fourteen"), (15, "fifteen"), (16, "sixteen")
           , (17, "seventeen"), (18, "eighteen"), (19, "nineteen") ]

tensPrefixesMap :: [(Integer, String)] -- Store prefix for e.g. 20, 30
tensPrefixesMap = [ (2, "twenty"), (3, "thirty"), (4, "forty"), (5, "fifty")
                  , (6, "sixty"), (7, "seventy"), (8, "eighty"), (9, "ninety") ]

sayOneDigit :: Integer -> Maybe String
sayOneDigit n = lookup n onesMap

sayTeen :: Integer -> Maybe String
sayTeen n = lookup n teensMap

sayTensPrefix :: Integer -> Maybe String -- Input is 2 for twenty, 3 for thirty etc.
sayTensPrefix n = lookup n tensPrefixesMap

-- Converts numbers 0-99 to English words.
sayUnder100 :: Integer -> Maybe String
sayUnder100 n
    | n < 0 || n > 99 = Nothing -- Out of this helper's range
    | n < 10          = sayOneDigit n
    | n < 20          = sayTeen n
    | otherwise = -- Numbers 20-99
        let tenVal = n `div` 10 -- For 23, tenVal is 2. For 50, tenVal is 5.
            oneVal = n `mod` 10 -- For 23, oneVal is 3. For 50, oneVal is 0.
        in case sayTensPrefix tenVal of
            Nothing -> Nothing -- Should not happen if tenVal is 2-9
            Just prefixStr ->
                if oneVal == 0
                then Just prefixStr -- e.g., "twenty", "fifty"
                else case sayOneDigit oneVal of
                         Nothing     -> Nothing -- Should not happen if oneVal is 1-9
                         Just oneStr -> Just (prefixStr ++ "-" ++ oneStr) -- e.g., "twenty-three"

-- Converts numbers 0-999 to English words.
sayThreeDigitNumber :: Integer -> Maybe String
sayThreeDigitNumber n
    | n < 0 || n > 999 = Nothing -- Out of this helper's range
    | n < 100          = sayUnder100 n
    | otherwise = -- Numbers 100-999
        let hundredsPlaceVal = n `div` 100 -- For 123, this is 1. For 700, this is 7.
            remainder        = n `mod` 100 -- For 123, this is 23. For 700, this is 0.
        in case sayOneDigit hundredsPlaceVal of
            Nothing -> Nothing -- Should not happen if hundredsPlaceVal is 1-9
            Just hundredDigitStr ->
                if remainder == 0
                then Just (hundredDigitStr ++ " hundred") -- e.g., "one hundred"
                else case sayUnder100 remainder of
                         Nothing         -> Nothing -- Should not happen if remainder is 1-99
                         Just remainderStr -> Just (hundredDigitStr ++ " hundred " ++ remainderStr) -- e.g., "one hundred twenty-three"

-- Breaks a number into three-digit chunks. E.g., 1234567 -> [1, 234, 567]
chunksOfThree :: Integer -> [Integer]
chunksOfThree n
    | n < 1000 = [n]
    | otherwise = chunksOfThree (n `div` 1000) ++ [n `mod` 1000]

-- Scale names: "", "thousand", "million", "billion".
-- These correspond to chunk groups from right to left.
scaleNamesList :: [String]
scaleNamesList = ["", "thousand", "million", "billion"]

inEnglish :: Integer -> Maybe String
inEnglish n
    | n < 0 || n > 999999999999 = Nothing -- Specified range
    | n == 0                     = Just "zero"   -- Special case for zero
    | otherwise =
        let numberChunks = chunksOfThree n
            numChunksLength = length numberChunks

            -- Convert a single chunk value to words, adding its scale word (e.g., "million") if applicable.
            -- chunkIdx is the 0-based index of the chunk from the left (most significant).
            processChunk :: (Integer, Int) -> Maybe String
            processChunk (chunkValue, chunkIdx)
                | chunkValue == 0 = Just "" -- Zero chunks (e.g., middle part of 1,000,000) add nothing.
                | otherwise =
                    -- Determine scale: rightmost chunk has no scale, then "thousand", "million", etc.
                    -- scaleListIdx is 0 for units, 1 for thousands, etc.
                    let scaleListIdx = numChunksLength - 1 - chunkIdx
                        -- Get scale word like "thousand", "million". Empty if it's the units chunk or scale is not needed.
                        scaleWord = if scaleListIdx > 0 && scaleListIdx < length scaleNamesList
                                    then scaleNamesList !! scaleListIdx
                                    else ""
                    in case sayThreeDigitNumber chunkValue of
                        Nothing -> Nothing -- Should not happen for chunkValue 0-999
                        Just chunkAsWords ->
                            if null scaleWord
                            then Just chunkAsWords
                            else Just (chunkAsWords ++ " " ++ scaleWord)
        in do
            -- Process all chunks. This results in a list of Maybe String.
            -- mapM sequences these Maybes: if any is Nothing, result is Nothing.
            -- Otherwise, Just [String], where some strings might be empty (for zero chunks).
            wordParts <- mapM processChunk (zip numberChunks [0..numChunksLength-1])
            
            -- Filter out empty strings (from zero-value chunks) and join with spaces.
            let nonEmptyWordParts = filter (not . null) wordParts
            -- For n > 0, nonEmptyWordParts will not be empty.
            Just (unwords nonEmptyWordParts)
