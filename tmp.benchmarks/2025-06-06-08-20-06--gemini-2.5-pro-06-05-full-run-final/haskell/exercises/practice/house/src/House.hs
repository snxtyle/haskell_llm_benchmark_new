module House (rhyme) where

import Data.List (intercalate)

rhyme :: String
rhyme = intercalate "\n\n" (map verse [0 .. length parts - 1]) ++ "\n"

parts :: [(String, String)]
parts =
  [ ("the house that Jack built.", "")
  , ("the malt", "that lay in")
  , ("the rat", "that ate")
  , ("the cat", "that killed")
  , ("the dog", "that worried")
  , ("the cow with the crumpled horn", "that tossed")
  , ("the maiden all forlorn", "that milked")
  , ("the man all tattered and torn", "that kissed")
  , ("the priest all shaven and shorn", "that married")
  , ("the rooster that crowed in the morn", "that woke")
  , ("the farmer sowing his corn", "that kept")
  , ("the horse and the hound and the horn", "that belonged to")
  ]

verse :: Int -> String
verse k = intercalate "\n" (firstLine : otherLines)
  where
    rhymeParts = reverse $ take (k + 1) parts
    firstLine = "This is " ++ fst (head rhymeParts)
    otherLines = zipWith line (init rhymeParts) (tail rhymeParts)
    line (_, verb) (nextSubject, _) = verb ++ " " ++ nextSubject
