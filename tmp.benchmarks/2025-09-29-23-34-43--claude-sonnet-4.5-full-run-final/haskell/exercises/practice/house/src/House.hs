module House (rhyme) where

import Data.List (intercalate)

-- Each element in the rhyme: (subject, action)
-- The action connects this subject to the previous one
rhymeData :: [(String, String)]
rhymeData =
  [ ("house that Jack built", "")
  , ("malt", "that lay in")
  , ("rat", "that ate")
  , ("cat", "that killed")
  , ("dog", "that worried")
  , ("cow with the crumpled horn", "that tossed")
  , ("maiden all forlorn", "that milked")
  , ("man all tattered and torn", "that kissed")
  , ("priest all shaven and shorn", "that married")
  , ("rooster that crowed in the morn", "that woke")
  , ("farmer sowing his corn", "that kept")
  , ("horse and the hound and the horn", "that belonged to")
  ]

-- Build a single verse for the nth element (1-indexed)
buildVerse :: Int -> String
buildVerse n = 
  if n == 1
    then "This is the " ++ fst (rhymeData !! 0) ++ "."
    else unlines $ firstLine : chainLines
  where
    firstLine = "This is the " ++ fst (rhymeData !! (n - 1))
    chainLines = buildChain (n - 1)
    
    -- Build the chain of connections from element (n-1) down to element 0
    buildChain :: Int -> [String]
    buildChain 0 = []
    buildChain i = 
      let (_, action) = rhymeData !! i
          prevSubject = fst (rhymeData !! (i - 1))
      in (action ++ " the " ++ prevSubject ++ if i == 1 then "." else "") : buildChain (i - 1)

-- Generate all verses and join them with blank lines
rhyme :: String
rhyme = intercalate "\n" (map buildVerse [1..length rhymeData]) ++ "\n"
