module House (rhyme) where

import Data.List (intercalate)

-- | The main noun phrase introduced in each verse.
subjects :: [String]
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

-- | The action that links each subject to the one that precedes it.
--   (The first element is a dummy and is never used.)
actions :: [String]
actions =
  [ ""  -- base case – not used directly
  , "that lay in "
  , "that ate "
  , "that killed "
  , "that worried "
  , "that tossed "
  , "that milked "
  , "that kissed "
  , "that married "
  , "that woke "
  , "that kept "
  , "that belonged to "
  ]

-- | Build a single verse by walking backwards through the accumulated
--   subjects/actions up to the requested index.
verse :: Int -> String
verse n =
  let firstLine = "This is " ++ subjects !! n
      tailLines =
        [ actions !! k ++ subjects !! (k - 1)
        | k <- [n, n - 1 .. 1]
        ]
  in unlines (firstLine : tailLines)

-- | The complete nursery rhyme.
rhyme :: String
rhyme = intercalate "\n" $ map verse [0 .. length subjects - 1]
