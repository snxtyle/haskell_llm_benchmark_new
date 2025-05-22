module House (rhyme) where

import Data.List (intercalate)

-- Each element in the rhyme with its subject and action
rhymeElements :: [(String, String)]
rhymeElements =
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
buildVerse n = "This is the " ++ buildChain (take n rhymeElements)
  where
    buildChain :: [(String, String)] -> String
    buildChain [] = ""
    buildChain [(subject, action)] = subject ++ " that " ++ action ++ "."
    buildChain ((subject, action):rest) = subject ++ "\nthat " ++ action ++ " the " ++ buildChain rest

-- Generate the complete rhyme
rhyme :: String
rhyme = intercalate "\n\n" [buildVerse n | n <- [1..length rhymeElements]]
