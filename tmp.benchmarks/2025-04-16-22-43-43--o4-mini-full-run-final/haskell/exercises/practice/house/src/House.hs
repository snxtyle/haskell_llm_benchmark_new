module House (rhyme) where

import Data.List (intercalate)

nouns :: [String]
nouns =
  [ "house"
  , "malt"
  , "rat"
  , "cat"
  , "dog"
  , "cow with the crumpled horn"
  , "maiden all forlorn"
  , "man all tattered and torn"
  , "priest all shaven and shorn"
  , "rooster that crowed in the morn"
  , "farmer sowing his corn"
  , "horse and the hound and the horn"
  ]

verbs :: [String]
verbs =
  [ "lay in the house that Jack built."
  , "ate the malt"
  , "killed the rat"
  , "worried the cat"
  , "tossed the dog"
  , "milked the cow with the crumpled horn"
  , "kissed the maiden all forlorn"
  , "married the man all tattered and torn"
  , "woke the priest all shaven and shorn"
  , "kept the rooster that crowed in the morn"
  , "belonged to the farmer sowing his corn"
  ]

verseLines :: Int -> [String]
verseLines 0 =
  [ "This is the house that Jack built." ]
verseLines k =
  ("This is the " ++ nouns !! k)
    : [ "that " ++ verbs !! j | j <- [k - 1, k - 2 .. 0] ]

rhyme :: String
rhyme =
  unlines
    $ intercalate [""] (map verseLines [0 .. length nouns - 1])
