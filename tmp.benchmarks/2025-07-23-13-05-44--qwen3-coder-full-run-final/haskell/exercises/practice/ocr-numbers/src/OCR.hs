module OCR (convert) where

convert :: String -> String
convert "" = ""
convert input
  | invalidGrid input = error "Invalid grid size"
  | otherwise = convertLines (lines input)
  where
    invalidGrid xs = 
      let ls = lines xs
          len = length ls
      in len `mod` 4 /= 0 || any (\l -> length l `mod` 3 /= 0) ls

convertLines :: [String] -> String
convertLines [] = ""
convertLines lines'
  | length lines' < 4 = ""
  | otherwise = 
      let (current, rest) = splitAt 4 lines'
          rowResult = convertRow current
      in if null rest 
         then rowResult
         else rowResult ++ "," ++ convertLines rest

convertRow :: [String] -> String
convertRow [r1, r2, r3, r4] = 
  let digits = [take 3 r1, take 3 r2, take 3 r3, take 3 r4]
      firstDigit = recognizeDigit digits
      restOfRow1 = drop 3 r1
      restOfRow2 = drop 3 r2
      restOfRow3 = drop 3 r3
      restOfRow4 = drop 3 r4
  in if null restOfRow1 && null restOfRow2 && null restOfRow3 && null restOfRow4
     then firstDigit
     else firstDigit ++ convertRow [restOfRow1, restOfRow2, restOfRow3, restOfRow4]
convertRow _ = ""

recognizeDigit :: [String] -> String
recognizeDigit [r1, r2, r3, r4] = 
  case (r1, r2, r3, r4) of
    (" _ ", "| |", "|_|", "   ") -> "0"
    ("   ", "  |", "  |", "   ") -> "1"
    (" _ ", " _|", "|_ ", "   ") -> "2"
    (" _ ", " _|", " _|", "   ") -> "3"
    ("   ", "|_|", "  |", "   ") -> "4"
    (" _ ", "|_ ", " _|", "   ") -> "5"
    (" _ ", "|_ ", "|_|", "   ") -> "6"
    (" _ ", "  |", "  |", "   ") -> "7"
    (" _ ", "|_|", "|_|", "   ") -> "8"
    (" _ ", "|_|", " _|", "   ") -> "9"
    _ -> "?"
recognizeDigit _ = "?"

splitAt :: Int -> [a] -> ([a], [a])
splitAt _ [] = ([], [])
splitAt n xs = (take n xs, drop n xs)
