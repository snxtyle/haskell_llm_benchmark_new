module PigLatin (translate) where

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord [] = ""
translateWord w@(c:cs) =
  if isVowel c || (length w >= 2 && take 2 w `elem` ["xr", "yt"])
  then w ++ "ay"
  else let prefix = findPrefix w
           rest = drop (length prefix) w
       in rest ++ prefix ++ "ay"

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

findPrefix :: String -> String
findPrefix w = go [] w
  where go acc [] = acc
        go acc (c:cs)
          | isVowel c = acc
          | c == 'y' && not (null acc) = acc
          | length (c:cs) >= 2 && take 2 (c:cs) == "qu" = go (acc ++ "qu") (drop 2 (c:cs))
          | otherwise = go (acc ++ [c]) cs
