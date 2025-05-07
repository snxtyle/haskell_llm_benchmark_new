module FoodChain (song) where

import Data.List (intercalate)

-- Defines the animals, their unique remarks/consequences, and order.
-- The remark for "fly" is the common refrain ending most verses.
-- The remark for "horse" is the song's final line.
animalsData :: [(String, String)]
animalsData = [
    ("fly",    "I don't know why she swallowed the fly. Perhaps she'll die."),
    ("spider", "It wriggled and jiggled and tickled inside her."),
    ("bird",   "How absurd to swallow a bird!"),
    ("cat",    "Imagine that, to swallow a cat!"),
    ("dog",    "What a hog, to swallow a dog!"),
    ("goat",   "Just opened her throat and swallowed a goat!"),
    ("cow",    "I don't know how she swallowed a cow!"),
    ("horse",  "She's dead, of course!")
    ]

-- Generates the cumulative "She swallowed the X to catch the Y" lines for a verse.
-- currentIndex: The index in `animalsData` of the animal most recently swallowed.
generateChaseLines :: Int -> [String]
generateChaseLines currentIndex =
    -- Iterate from the current animal down to the one before the fly.
    map buildLine [currentIndex, currentIndex - 1 .. 1]
  where
    buildLine i =
        let (swallowedAnimal, _) = animalsData !! i      -- The animal swallowed in this line
            (preyAnimal, _)      = animalsData !! (i - 1) -- The animal it was swallowed to catch
            lineStart            = "She swallowed the " ++ swallowedAnimal ++ " to catch the " ++ preyAnimal
        -- Special text if the prey is the spider
        in if preyAnimal == "spider"
           then lineStart ++ " that wriggled and jiggled and tickled inside her."
           else lineStart ++ "."

-- Generates a single verse of the song.
-- verseIndex: The index in `animalsData` for the animal featured in this verse.
generateVerse :: Int -> String
generateVerse verseIndex =
    let (animalName, remark) = animalsData !! verseIndex
        firstLine = "I know an old lady who swallowed a " ++ animalName ++ "."
    in
    if animalName == "horse"
    then -- The horse verse is unique and ends the song.
        intercalate "\n" [firstLine, remark]
    else
        -- Standard verse structure
        let verseLines =
                -- "I know an old lady who swallowed a [animal]."
                [firstLine] ++
                -- Add the animal's unique remark, unless it's the fly (its "remark" is the refrain).
                (if animalName == "fly" then [] else [remark]) ++
                -- Add the "She swallowed the X to catch the Y..." lines.
                generateChaseLines verseIndex ++
                -- Add the common refrain (which is stored as the fly's "remark").
                [snd (animalsData !! 0)]
        in intercalate "\n" verseLines

-- Generates the full song lyrics by creating each verse, joining them,
-- and ensuring a final trailing newline.
song :: String
song =
    let verses = map generateVerse [0 .. length animalsData - 1]
        fullSongContent = intercalate "\n\n" verses
    -- The test expects a single trailing newline at the end of the entire song.
    in if null verses
       then "" -- Should not happen with current animalsData
       else fullSongContent ++ "\n"
