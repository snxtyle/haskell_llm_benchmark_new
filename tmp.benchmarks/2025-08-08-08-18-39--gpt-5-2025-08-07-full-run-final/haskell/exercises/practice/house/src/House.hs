module House (rhyme) where

import Data.List (intercalate)

rhyme :: String
rhyme = intercalate "\n" $ map (unlines . verse) [0 .. lastIndex]
  where
    lastIndex = length subjects - 1

verse :: Int -> [String]
verse i = header : tails
  where
    header = "This is " ++ subjects !! i
    tails
      | i <= 0    = []
      | otherwise = [ "that " ++ verbs !! k ++ " " ++ subjects !! k | k <- [i-1, i-2 .. 0] ]

subjects :: [String]
subjects =
  [ "the house that Jack built."
  , "the malt"
  , "the rat"
  , "the cat"
  , "the dog"
  , "the cow with the crumpled horn"
  , "the maiden all forlorn"
  , "the man all tattered and torn"
  , "the priest all shaven and shorn"
  , "the rooster that crowed in the morn"
  , "the farmer sowing his corn"
  , "the horse and the hound and the horn"
  ]

verbs :: [String]
verbs =
  [ "lay in"
  , "ate"
  , "killed"
  , "worried"
  , "tossed"
  , "milked"
  , "kissed"
  , "married"
  , "woke"
  , "kept"
  , "belonged to"
  ]
