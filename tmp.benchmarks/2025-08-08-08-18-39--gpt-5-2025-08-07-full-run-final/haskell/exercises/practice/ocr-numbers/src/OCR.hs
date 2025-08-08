module OCR (convert) where

import Data.List (intercalate)

convert :: String -> String
convert xs =
  let ls = lines xs
  in validateAndConvert ls
  where
    validateAndConvert :: [String] -> String
    validateAndConvert [] = error "invalid input"
    validateAndConvert ls
      | any (/= width) (map length ls) = error "invalid input"
      | height `mod` 4 /= 0            = error "invalid input"
      | width == 0                     = error "invalid input"
      | width `mod` 3 /= 0             = error "invalid input"
      | otherwise =
          intercalate "," $ map convertBlock (chunk 4 ls)
      where
        height = length ls
        width  = length (head ls)

    convertBlock :: [String] -> String
    convertBlock fourLines =
      let w = length (head fourLines)
          positions = [0,3..w-3]
          glyphs = [ map (take 3 . drop i) fourLines | i <- positions ]
      in map lookupGlyph glyphs

    lookupGlyph :: [String] -> Char
    lookupGlyph g =
      case lookup g digitPatterns of
        Just c  -> c
        Nothing -> '?'

    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = take n xs : chunk n (drop n xs)

    digitPatterns :: [([String], Char)]
    digitPatterns =
      [ ([" _ "
        ,"| |"
        ,"|_|"
        ,"   "], '0')
      , (["   "
        ,"  |"
        ,"  |"
        ,"   "], '1')
      , ([" _ "
        ," _|"
        ,"|_ "
        ,"   "], '2')
      , ([" _ "
        ," _|"
        ," _|"
        ,"   "], '3')
      , (["   "
        ,"|_|"
        ,"  |"
        ,"   "], '4')
      , ([" _ "
        ,"|_ "
        ," _|"
        ,"   "], '5')
      , ([" _ "
        ,"|_ "
        ,"|_|"
        ,"   "], '6')
      , ([" _ "
        ,"  |"
        ,"  |"
        ,"   "], '7')
      , ([" _ "
        ,"|_|"
        ,"|_|"
        ,"   "], '8')
      , ([" _ "
        ,"|_|"
        ," _|"
        ,"   "], '9')
      ]
