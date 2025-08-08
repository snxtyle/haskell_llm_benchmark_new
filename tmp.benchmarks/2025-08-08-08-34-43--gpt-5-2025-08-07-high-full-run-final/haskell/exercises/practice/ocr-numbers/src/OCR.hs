module OCR (convert) where

import Data.List (transpose, intercalate)

convert :: String -> String
convert input =
  let rs = lines input
  in case validate rs of
       () -> intercalate "," (map decodeBlock (chunksOf 4 rs))
  where
    validate :: [String] -> ()
    validate rows =
      let h = length rows
      in if h `mod` 4 /= 0
            then error "invalid input"
         else
           let widths = map length rows
               w = case widths of
                     []    -> 0
                     (x:_) -> x
           in if any (/= w) widths
                 then error "invalid input"
              else if w `mod` 3 /= 0
                    then error "invalid input"
                   else ()

    decodeBlock :: [String] -> String
    decodeBlock block =
      let glyphs = transpose (map (chunksOf 3) block)
      in map decodeGlyph glyphs

    decodeGlyph :: [String] -> Char
    decodeGlyph gl =
      let key = concat gl
      in case lookup key digitPatterns of
           Just c  -> c
           Nothing -> '?'


digitPatterns :: [(String, Char)]
digitPatterns =
  [ (concat [" _ ","| |","|_|","   "], '0')
  , (concat ["   ","  |","  |","   "], '1')
  , (concat [" _ "," _|","|_ ","   "], '2')
  , (concat [" _ "," _|"," _|","   "], '3')
  , (concat ["   ","|_|","  |","   "], '4')
  , (concat [" _ ","|_ "," _|","   "], '5')
  , (concat [" _ ","|_ ","|_|","   "], '6')
  , (concat [" _ ","  |","  |","   "], '7')
  , (concat [" _ ","|_|","|_|","   "], '8')
  , (concat [" _ ","|_|"," _|","   "], '9')
  ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | n <= 0    = error "invalid chunk size"
  | null xs   = []
  | otherwise = take n xs : chunksOf n (drop n xs)
