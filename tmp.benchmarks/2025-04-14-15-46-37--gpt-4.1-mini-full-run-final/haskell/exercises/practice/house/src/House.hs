module House (rhyme) where

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

rhyme :: String
rhyme = unlines $ concatMap verse [0 .. length parts - 1]
  where
    verse n =
      let (subject, _) = parts !! n
          linesForVerse = ("This is " ++ subject) : buildLines n
       in linesForVerse ++ [""]

    buildLines 0 = []
    buildLines n =
      let (subjectPrev, verb) = parts !! (n - 1)
       in (verb ++ " " ++ subjectPrev) : buildLines (n - 1)
