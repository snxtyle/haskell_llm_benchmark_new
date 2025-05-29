module OCR (convert) where

import Data.List (intercalate)

convert :: String -> String
convert input = 
  let ls = lines input
      n = length ls
  in if n `mod` 4 /= 0
       then error "Number of lines not divisible by 4"
       else 
         let groups = chunksOf 4 ls
             processGroup group = 
                let [l1, l2, l3, l4] = group
                    len = length l1
                in if any (\l -> length l /= len) [l2, l3, l4]
                   then error "Inconsistent line lengths in group"
                   else if len `mod` 3 /= 0
                        then error "Line length not divisible by 3"
                        else 
                          let digitCount = len `div` 3
                              digits = [ recognize [ take 3 (drop (3*i) l1), 
                                                    take 3 (drop (3*i) l2), 
                                                    take 3 (drop (3*i) l3) ] 
                                       | i <- [0..digitCount-1] ]
                          in digits
         in intercalate "," (map processGroup groups)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

recognize :: [String] -> Char
recognize [" _ ", "| |", "|_|"] = '0'
recognize ["   ", "  |", "  |"] = '1'
recognize [" _ ", " _|", "|_ "] = '2'
recognize [" _ ", " _|", " _|"] = '3'
recognize ["   ", "|_|", "  |"] = '4'
recognize [" _ ", "|_ ", " _|"] = '5'
recognize [" _ ", "|_ ", "|_|"] = '6'
recognize [" _ ", "  |", "  |"] = '7'
recognize [" _ ", "|_|", "|_|"] = '8'
recognize [" _ ", "|_|", " _|"] = '9'
recognize _ = '?'
