module House (rhyme) where

import Data.List (intercalate)

-- | Full rhyme, exactly matching the canonical text (including the
-- | final newline).
rhyme :: String
rhyme = intercalate "\n" (map verse [0 .. lastIdx])
  where
    lastIdx = length parts - 1

-- | Build one verse (0‑based index).
verse :: Int -> String
verse i =
  unlines $
    ("This is " ++ subject i)
      : [ "that " ++ action j ++ " " ++ subject (j - 1)
        | j <- [i, i - 1 .. 1]
        ]
  where
    subject n = fst (parts !! n)
    action  n = snd (parts !! n)

-- | (subject, action) pairs; the first action is never used.
parts :: [(String, String)]
parts =
  [ ("the house that Jack built.", "")
  , ("the malt", "lay in")
  , ("the rat", "ate")
  , ("the cat", "killed")
  , ("the dog", "worried")
  , ("the cow with the crumpled horn", "tossed")
  , ("the maiden all forlorn", "milked")
  , ("the man all tattered and torn", "kissed")
  , ("the priest all shaven and shorn", "married")
  , ("the rooster that crowed in the morn", "woke")
  , ("the farmer sowing his corn", "kept")
  , ("the horse and the hound and the horn", "belonged to")
  ]
