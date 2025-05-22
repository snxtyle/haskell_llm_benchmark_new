module House (rhyme) where

rhyme :: String
rhyme = unlines $ map buildVerse [0..11]

-- The characters and their actions in order
characters :: [(String, String)]
characters = 
  [ ("house", "that Jack built")
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

-- Build a single verse for the given index
buildVerse :: Int -> String
buildVerse n = unlines $ buildLines (n + 1)
  where
    buildLines :: Int -> [String]
    buildLines 1 = ["This is the " ++ fst (characters !! 0) ++ " " ++ snd (characters !! 0) ++ "."]
    buildLines len = 
      let (char, _) = characters !! (len - 1)
          firstLine = "This is the " ++ char
          restLines = map buildActionLine [len-1, len-2 .. 1]
      in firstLine : restLines
    
    buildActionLine :: Int -> String
    buildActionLine 1 = snd (characters !! 0) ++ "."
    buildActionLine idx = 
      let (prevChar, action) = characters !! (idx - 1)
      in action ++ " the " ++ prevChar
