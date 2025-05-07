module House (rhyme) where

import Data.List (intercalate)

rhyme :: String
rhyme = intercalate "\n\n" $ map verse [1..length pieces]
  where
    verse n = "This is " ++ intercalate "\n" (map fst $ take n $ drop (length pieces - n) pieces)
    pieces =
      [ ("the house that Jack built.", "")
      , ("the malt", "that lay in the house that Jack built.")
      , ("the rat", "that ate the malt")
      , ("the cat", "that killed the rat")
      , ("the dog", "that worried the cat")
      , ("the cow with the crumpled horn", "that tossed the dog")
      , ("the maiden all forlorn", "that milked the cow with the crumpled horn")
      , ("the man all tattered and torn", "that kissed the maiden all forlorn")
      , ("the priest all shaven and shorn", "that married the man all tattered and torn")
      , ("the rooster that crowed in the morn", "that woke the priest all shaven and shorn")
      , ("the farmer sowing his corn", "that kept the rooster that crowed in the morn")
      , ("the horse and the hound and the horn", "that belonged to the farmer sowing his corn")
      ]

    -- Build the verse by embedding each line
    verse 1 = "This is " ++ (fst $ head pieces)
    verse n = "This is " ++ (fst $ pieces !! (n-1)) ++ "\n" ++
              (intercalate "\n" $ map snd $ drop (length pieces - n + 1) pieces)
