module House (rhyme) where

import Data.List (intercalate)

rhyme :: String
rhyme = intercalate "\n\n" (map verse [0 .. length subjects - 1])
  where
    verse i =
      unlines $
        ("This is " ++ subjects !! i)
          : map ("that " ++) (concatMap (lines . actions) (reverse [1 .. i]))

-- Subject line for each verse (without trailing period)
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

-- For each index i (>0), describes how subject i relates to subject i-1.
-- Return may span multiple lines; consumers will prefix each line with "that ".
actions :: Int -> String
actions 0 = "" -- no action for the first verse
actions 1 = "lay in the house that Jack built."
actions 2 = "ate the malt\nlay in the house that Jack built."
actions 3 = "killed the rat\nate the malt\nlay in the house that Jack built."
actions 4 = "worried the cat\nkilled the rat\nate the malt\nlay in the house that Jack built."
actions 5 = "tossed the dog\nworried the cat\nkilled the rat\nate the malt\nlay in the house that Jack built."
actions 6 = "milked the cow with the crumpled horn\ntossed the dog\nworried the cat\nkilled the rat\nate the malt\nlay in the house that Jack built."
actions 7 = "kissed the maiden all forlorn\nmilked the cow with the crumpled horn\ntossed the dog\nworried the cat\nkilled the rat\nate the malt\nlay in the house that Jack built."
actions 8 = "married the man all tattered and torn\nkissed the maiden all forlorn\nmilked the cow with the crumpled horn\ntossed the dog\nworried the cat\nkilled the rat\nate the malt\nlay in the house that Jack built."
actions 9 = "woke the priest all shaven and shorn\nmarried the man all tattered and torn\nkissed the maiden all forlorn\nmilked the cow with the crumpled horn\ntossed the dog\nworried the cat\nkilled the rat\nate the malt\nlay in the house that Jack built."
actions 10 = "kept the rooster that crowed in the morn\nwoke the priest all shaven and shorn\nmarried the man all tattered and torn\nkissed the maiden all forlorn\nmilked the cow with the crumpled horn\ntossed the dog\nworried the cat\nkilled the rat\nate the malt\nlay in the house that Jack built."
actions 11 = "belonged to the farmer sowing his corn\nkept the rooster that crowed in the morn\nwoke the priest all shaven and shorn\nmarried the man all tattered and torn\nkissed the maiden all forlorn\nmilked the cow with the crumpled horn\ntossed the dog\nworried the cat\nkilled the rat\nate the malt\nlay in the house that Jack built."
actions _ = error "Unexpected verse index"
