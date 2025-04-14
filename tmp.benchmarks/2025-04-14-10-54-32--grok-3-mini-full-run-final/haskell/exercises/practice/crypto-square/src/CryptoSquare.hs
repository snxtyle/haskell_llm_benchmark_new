module CryptoSquare (encode) where

import Data.Char (isAlpha, toLower)
import Data.List (genericLength, genericTake, genericDrop)  -- For generic operations on lists

encode :: String -> String
encode xs
  | null normalized = ""  -- Handle empty input
  | otherwise = unwords processedColumns
  where
    -- Step 1: Normalize the string
    normalized = map toLower $ filter isAlpha xs  -- Remove non-alphabetic chars and downcase
    n = length normalized
    
    -- Step 2: Calculate c and r
    sqrtN = sqrt (fromIntegral n)  -- sqrt from Prelude
    cInitial = ceiling sqrtN  -- Ceiling from Prelude
    c = findSuitableC cInitial  -- Find the smallest c that satisfies conditions
    r = ceiling (fromIntegral n / fromIntegral c)  -- r = ceil(n / c)
    
    -- Helper to find c
    findSuitableC start = head [c' | c' <- [start..], let r' = ceiling (fromIntegral n / fromIntegral c'), c' >= r' && c' - r' <= 1]
    
    -- Step 3: Pad the string
    paddedLength = r * c
    padded = take paddedLength (normalized ++ repeat ' ')  -- Pad with spaces
    
    -- Step 4: Form the grid (list of rows)
    rows = [take c (drop (i * c) padded) | i <- [0..r-1]]  -- Each row is a String of c chars
    
    -- Step 5: Extract columns
    columns = [[rows !! i !! j | i <- [0..r-1]] | j <- [0..c-1]]  -- List of lists for columns
    columnStrings = columns  -- Fixed: Directly use columns as it's already the list of strings
    
    -- Step 6: Determine padding for last n chunks
    padCount = paddedLength - n  -- Number of padding characters
    processedColumns = [if i >= c - padCount then columnStrings !! i ++ " " else columnStrings !! i | i <- [0..c-1]]
