module CryptoSquare (encode) where

import Data.Char (isAlphaNum, toLower)
import Data.List (transpose, unwords)

-- Helper function to split a list into chunks of a given size.
-- Similar to chunksOf from Data.List.Split, but implemented manually
-- to avoid adding dependencies beyond 'base'.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | n <= 0    = [xs] -- Avoid infinite loop or error on non-positive n
  | otherwise = take n xs : chunksOf n (drop n xs)

-- Normalizes the input string: lowercase and only alphanumeric characters.
normalize :: String -> String
normalize = filter isAlphaNum . map toLower

-- Calculates the dimensions (rows, columns) of the crypto square.
dimensions :: Int -> (Int, Int) -- (rows, cols)
dimensions len
  | len == 0  = (0, 0) -- Handle empty normalized string
  | otherwise =
      let lenF = fromIntegral len :: Double
          -- Calculate initial column estimate based on square root
          cFloat = sqrt lenF
          -- Find the smallest integer c such that c >= sqrt(len)
          c = ceiling cFloat :: Int
          -- Find the smallest integer r such that r >= len / c
          r = ceiling (lenF / fromIntegral c) :: Int
      -- Ensure c >= r and c - r <= 1. The calculation above might not
      -- directly satisfy c - r <= 1, but it finds the smallest rectangle.
      -- The problem description's constraints (c >= r, c - r <= 1)
      -- uniquely determine c and r for a given length.
      -- Let's re-calculate based on the constraints directly.
      in findDimensions len 1 1
        where
          findDimensions l rows cols
            | rows * cols >= l && cols >= rows && cols - rows <= 1 = (rows, cols)
            | rows * cols >= l && cols >= rows = findDimensions l (rows + 1) cols -- Increase rows if cols too big relative to rows
            | otherwise = findDimensions l rows (cols + 1) -- Increase cols otherwise

-- Encodes the input string using the Crypto Square method.
encode :: String -> String
encode xs = -- Added '=' here
  -- Normalize the input text first.
  let normalized = normalize xs
      len = length normalized
  in if len == 0
     -- If normalized text is empty, return an empty string.
     then ""
     else
       -- Calculate the rectangle dimensions (rows r, columns c).
       let (r, c) = dimensions len
           -- Calculate the total size of the rectangle.
           targetLen = r * c
           -- Calculate the number of padding spaces needed.
           padding = targetLen - len
           -- Pad the normalized string with spaces to fit the rectangle.
           padded = normalized ++ replicate padding ' '
           -- Arrange the padded string into rows of length c.
           rectangle = chunksOf c padded
           -- Transpose the rectangle to read columns.
           transposedColumns = transpose rectangle
           -- Concatenate the characters from the transposed columns.
           -- encodedChars = concat transposedColumns -- This was incorrect for spacing
           -- Chunk the transposed columns (which are now rows of length r)
           -- and join them with spaces.
           outputChunks = map (\col -> take r col) transposedColumns -- Ensure each chunk has length r
       -- Join the chunks with spaces for the final output.
       in unwords outputChunks
