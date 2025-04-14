module House (rhyme) where

import Data.List (intercalate)

-- The unique subjects introduced in each verse.
subjects :: [String]
subjects =
    [ "the house that Jack built"
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

-- The verb phrases connecting the subject of verse n to the subject of verse n-1.
-- The first element is unused as the first verse has no preceding connection.
verbs :: [String]
verbs =
    [ ""
    , "lay in"
    , "ate"
    , "killed"
    , "worried"
    , "tossed"
    , "milked"
    , "kissed"
    , "married"
    , "woke"
    , "kept"
    , "belonged to"
    ]

-- Generates the recursive lines for a given verse (0-indexed).
-- e.g., for verse 2 ("the rat"), it generates:
-- ["that ate the malt", "that lay in the house that Jack built."]
verseTail :: Int -> [String]
verseTail 0 = [] -- The first verse has no preceding lines.
verseTail 1 = ["that " ++ (verbs !! 1) ++ " " ++ (subjects !! 0) ++ "."] -- The second verse's tail is the base case, ending the sentence.
verseTail n = ("that " ++ (verbs !! n) ++ " " ++ (subjects !! (n-1))) : verseTail (n-1) -- Build line and recurse.

-- Builds the complete text for a single verse (0-indexed).
buildVerse :: Int -> String
buildVerse 0 = "This is " ++ (subjects !! 0) ++ "." -- The first verse is a special case.
-- Use intercalate "\n" instead of unlines to avoid trailing newline on each verse.
buildVerse n = intercalate "\n" $ ("This is " ++ (subjects !! n)) : verseTail n -- Combine the intro line with the recursive tail.

-- Generates the entire nursery rhyme by building each verse and joining them.
rhyme :: String
rhyme = intercalate "\n\n" $ map buildVerse [0 .. length subjects - 1]
