module House (rhyme) where

import Data.List (intercalate)

rhyme :: String
rhyme = intercalate "\n" (map stanza [0 .. lastIndex])
  where
    lastIndex = length subjects - 1
    stanza i =
      unlines $
        ("This is " ++ subjects !! i)
          : [ verbs !! (j - 1) ++ " " ++ subjects !! (j - 1)
            | j <- reverse [1 .. i]
            ]

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

    verbs =
      [ "that lay in"
      , "that ate"
      , "that killed"
      , "that worried"
      , "that tossed"
      , "that milked"
      , "that kissed"
      , "that married"
      , "that woke"
      , "that kept"
      , "that belonged to"
      ]
