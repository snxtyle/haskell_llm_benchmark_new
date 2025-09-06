module House (rhyme) where

rhyme :: String
rhyme = unlines $ map buildVerse [1..12]

-- Build each verse of the rhyme
buildVerse :: Int -> String
buildVerse n = unlines $ ("This is " ++ subject 0) : buildLines 1
  where
    buildLines :: Int -> [String]
    buildLines i
      | i >= n = []
      | otherwise = ("that " ++ verb i ++ " " ++ subject i) : buildLines (i + 1)

-- List of subjects for each line
subject :: Int -> String
subject 0 = "the house that Jack built."
subject 1 = "the malt"
subject 2 = "the rat"
subject 3 = "the cat"
subject 4 = "the dog"
subject 5 = "the cow with the crumpled horn"
subject 6 = "the maiden all forlorn"
subject 7 = "the man all tattered and torn"
subject 8 = "the priest all shaven and shorn"
subject 9 = "the rooster that crowed in the morn"
subject 10 = "the farmer sowing his corn"
subject 11 = "the horse and the hound and the horn"
subject _ = ""

-- List of verbs for each line (starting from line 2)
verb :: Int -> String
verb 1 = "lay in"
verb 2 = "ate"
verb 3 = "killed"
verb 4 = "worried"
verb 5 = "tossed"
verb 6 = "milked"
verb 7 = "kissed"
verb 8 = "married"
verb 9 = "woke"
verb 10 = "kept"
verb 11 = "belonged to"
verb _ = ""
