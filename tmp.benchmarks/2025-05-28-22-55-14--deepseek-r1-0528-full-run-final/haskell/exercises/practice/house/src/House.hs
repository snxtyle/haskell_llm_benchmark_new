module House (rhyme) where

import Data.List (intercalate)

rhyme :: String
rhyme = intercalate "\n\n" (map verse [0..11]) ++ "\n"

verse :: Int -> String
verse n = unlines body ++ "."
  where
    body = ("This is " ++ subjects !! n) : 
           [ "that " ++ verbs !! i ++ " " ++ subjects !! i | i <- [n-1, n-2 .. 0] ]

    subjects :: [String]
    subjects = 
      [ "the house that Jack built"
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
