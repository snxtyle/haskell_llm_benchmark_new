module House (rhyme) where

rhyme :: String
rhyme = unlines $ map verse [0..11]

verse :: Int -> String
verse n = "This is " ++ buildVerse n

buildVerse :: Int -> String
buildVerse n = intercalate "\n" (reverse $ take (n + 1) parts)

parts :: [String]
parts = [
  "the house that Jack built.",
  "the malt\nthat lay in",
  "the rat\nthat ate",
  "the cat\nthat killed",
  "the dog\nthat worried",
  "the cow with the crumpled horn\nthat tossed",
  "the maiden all forlorn\nthat milked",
  "the man all tattered and torn\nthat kissed",
  "the priest all shaven and shorn\nthat married",
  "the rooster that crowed in the morn\nthat woke",
  "the farmer sowing his corn\nthat kept",
  "the horse and the hound and the horn\nthat belonged to"
  ]

intercalate :: [a] -> [[a]] -> [a]
intercalate _ [] = []
intercalate sep (x:xs) = x ++ concatMap (sep ++) xs
