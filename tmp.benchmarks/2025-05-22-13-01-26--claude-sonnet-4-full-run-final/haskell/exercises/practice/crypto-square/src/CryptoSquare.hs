module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)

encode :: String -> String
encode xs = 
    let normalized = normalize xs
        len = length normalized
        (rows, cols) = calculateDimensions len
        paddedText = padText normalized (rows * cols)
        rectangle = chunksOf cols paddedText
        columns = transpose rectangle
        encodedChunks = map (padChunk rows) columns
    in unwords encodedChunks

normalize :: String -> String
normalize = map toLower . filter isAlphaNum

calculateDimensions :: Int -> (Int, Int)
calculateDimensions len = 
    let c = ceiling (sqrt (fromIntegral len))
        r = if c * (c - 1) >= len then c - 1 else c
    in (r, c)

padText :: String -> Int -> String
padText text targetLen = text ++ replicate (targetLen - length text) ' '

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])

padChunk :: Int -> String -> String
padChunk targetLen chunk = 
    let actualLen = length chunk
    in if actualLen < targetLen 
       then chunk ++ " "
       else chunk
