module House (rhyme) where

import Data.List (intercalate)

-- Each element in the rhyme with its subject and action
elements :: [(String, String)]
elements =
  [ ("house", "Jack built")
  , ("malt", "lay in")
  , ("rat", "ate")
  , ("cat", "killed")
  , ("dog", "worried")
  , ("cow with the crumpled horn", "tossed")
  , ("maiden all forlorn", "milked")
  , ("man all tattered and torn", "kissed")
  , ("priest all shaven and shorn", "married")
  , ("rooster that crowed in the morn", "woke")
  , ("farmer sowing his corn", "kept")
  , ("horse and the hound and the horn", "belonged to")
  ]

-- Build a single verse of the rhyme
buildVerse :: Int -> String
buildVerse n = "This is the " ++ buildLines (take n elements)
  where
    buildLines :: [(String, String)] -> String
    buildLines [] = ""
    buildLines [(subj, action)] = subj ++ " that " ++ action ++ "."
    buildLines ((subj, action):rest) = 
      subj ++ "\nthat " ++ action ++ " the " ++ buildLines rest

-- Generate the complete rhyme
rhyme :: String
rhyme = intercalate "\n" verses
  where
    verses = [buildVerse n | n <- [1..length elements]]
