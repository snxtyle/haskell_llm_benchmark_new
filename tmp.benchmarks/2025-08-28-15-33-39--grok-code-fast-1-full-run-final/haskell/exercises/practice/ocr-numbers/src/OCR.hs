module OCR (convert) where

import Data.List (intercalate)

convert :: String -> String
convert input =
  let ls = lines input
  in if length ls `mod` 4 /= 0 then error "invalid input size"
     else let groups = chunk 4 ls
          in intercalate "," $ map convertGroup groups

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)

convertGroup :: [String] -> String
convertGroup group =
  let w = length (head group)
  in if null group || any ((/= w) . length) group || w `mod` 3 /= 0 then "?"
     else let n = w `div` 3
              digits = map (recognize . extract group) [0..n-1]
          in concat digits

extract :: [String] -> Int -> String
extract group i = concat [take 3 $ drop (3 * i) l | l <- group]

recognize :: String -> Char
recognize s = case lookup s patterns of
                Just c -> c
                Nothing -> '?'

patterns :: [(String, Char)]
patterns = [
  (" _ | ||_|   ", '0'),
  ("     |  |   ", '1'),
  (" _  _||_    ", '2'),
  (" _  _| _|   ", '3'),
  ("   |_|  |   ", '4'),
  (" _ |_  _|   ", '5'),
  (" _ |_ |_|   ", '6'),
  (" _    |  |   ", '7'),
  (" _ |_| |_|   ", '8'),
  (" _ |_|  _|   ", '9')
]
