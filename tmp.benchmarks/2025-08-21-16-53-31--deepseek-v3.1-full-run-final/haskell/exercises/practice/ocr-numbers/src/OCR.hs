module OCR (convert) where

import Data.List (transpose)

-- Define the patterns for each digit
digitPatterns :: [(String, Char)]
digitPatterns =
    [ (" _ | ||_|   ", '0')
    , ("     |  |   ", '1')
    , (" _  _||_    ", '2')
    , (" _  _| _|   ", '3')
    , ("   |_|  |   ", '4')
    , (" _ |_  _|   ", '5')
    , (" _ |_ |_|   ", '6')
    , (" _   |  |   ", '7')
    , (" _ |_||_|   ", '8')
    , (" _ |_| _|   ", '9')
    ]

-- Custom chunksOf implementation
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

convert :: String -> String
convert input = 
    let linesOfInput = lines input
        -- Group into chunks of 4 lines each (each number block)
        numberBlocks = chunksOf 4 linesOfInput
        -- Convert each block to a string of digits
        convertedBlocks = map convertBlock numberBlocks
    in intercalate "," convertedBlocks
    where
        intercalate :: [a] -> [[a]] -> [a]
        intercalate _ [] = []
        intercalate sep (x:xs) = x ++ concatMap (sep ++) xs

convertBlock :: [String] -> String
convertBlock lines
    | length lines < 3 = error "Invalid input size - not enough lines"
    | any (\line -> length line `mod` 3 /= 0) (take 3 lines) = error "Invalid input size"
    | otherwise = map (recognizeDigit . getDigitPattern) [0,3..maxCol-1]
    where
        maxCol = maximum (map length (take 3 lines))
        paddedLines = map (padTo maxCol) (take 3 lines)
        
        getDigitPattern :: Int -> String
        getDigitPattern startCol = concatMap (take 3 . drop startCol) paddedLines
        
        padTo :: Int -> String -> String
        padTo n s = s ++ replicate (n - length s) ' '

recognizeDigit :: String -> Char
recognizeDigit pattern =
    case lookup pattern digitPatterns of
        Just digit -> digit
        Nothing -> '?'
