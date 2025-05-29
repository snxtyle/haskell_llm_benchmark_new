module PigLatin (translate) where

import Data.List (isPrefixOf, findIndex)

translate :: String -> String
translate = unwords . map translateWord . words

translateWord :: String -> String
translateWord word
  | null word = word
  | "xr" `isPrefixOf` word || "yt" `isPrefixOf` word = word ++ "ay"
  | head word `elem` "aeiou" = word ++ "ay"
  | "qu" `isPrefixOf` word = 
      let (cluster, rest) = splitAt 2 word
      in rest ++ cluster ++ "ay"
  | otherwise = 
      case findIndex (== 'y') word of
        Just i | i > 0 && all (\c -> c `notElem` "aeiou") (take i word) ->
          let (cluster, rest) = splitAt i word
          in rest ++ cluster ++ "ay"
        _ -> 
          let cluster1 = takeWhile (\c -> c `notElem` "aeiou") word
              n = length cluster1
          in if not (null cluster1) && "qu" `isPrefixOf` drop n word
             then let cluster2 = cluster1 ++ "qu"
                      rest = drop (n+2) word
                  in rest ++ cluster2 ++ "ay"
             else let (cluster, rest) = splitAt n word
                  in rest ++ cluster ++ "ay"
