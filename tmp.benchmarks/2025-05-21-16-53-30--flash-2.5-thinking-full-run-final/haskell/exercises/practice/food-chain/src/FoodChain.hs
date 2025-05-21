module FoodChain (song) where

import Data.List (intercalate)

-- Defines the data for each animal: (name, unique_line, caught_phrase)
-- The 'caught_phrase' is appended to the "to catch the [animal]" part if applicable.
animalDetails :: [(String, String, String)]
animalDetails =
  [ ("fly", "I don't know why she swallowed the fly. Perhaps she'll die.", "")
  , ("spider", "It wriggled and jiggled and tickled inside her.", " that wriggled and jiggled and tickled inside her") -- Removed trailing period
  , ("bird", "How absurd to swallow a bird!", "")
  , ("cat", "Imagine that, to swallow a cat!", "")
  , ("dog", "What a hog, to swallow a dog!", "")
  , ("goat", "Just opened her throat and swallowed a goat!", "")
  , ("cow", "I don't know how she swallowed a cow!", "")
  , ("horse", "She's dead, of course!", "")
  ]

-- Helper to get animal name by index
getName :: Int -> String
getName i = (\(n, _, _) -> n) (animalDetails !! i)

-- Helper to get animal's unique descriptive line by index
getUniqueLine :: Int -> String
getUniqueLine i = (\(_, u, _) -> u) (animalDetails !! i)

-- Helper to get animal's special phrase when caught by another animal
getCaughtPhrase :: Int -> String
getCaughtPhrase i = (\(_, _, c) -> c) (animalDetails !! i)

-- Generates the cumulative lines for a given verse index 'n'.
-- These are the "She swallowed the X to catch the Y" lines.
generateCumulativeLines :: Int -> [String]
generateCumulativeLines n =
  [ "She swallowed the " ++ (getName i) ++ " to catch the " ++ (getName (i-1)) ++ (getCaughtPhrase (i-1)) ++ "."
  | i <- reverse [1..n]
  ]

-- Generates a single verse as a list of lines.
verseLines :: Int -> [String]
verseLines n =
  let
    currentAnimalName = getName n
    currentUniqueLine = getUniqueLine n
    initialLine = "I know an old lady who swallowed a " ++ currentAnimalName ++ "."
  in
    case currentAnimalName of
      "fly" ->
        [initialLine, currentUniqueLine]
      "horse" ->
        [initialLine, currentUniqueLine]
      _ ->
        let
          cumulative = generateCumulativeLines n
        in
          [initialLine, currentUniqueLine] ++ cumulative ++ ["I don't know why she swallowed the fly. Perhaps she'll die."]

-- The main song function that composes all verses.
song :: String
song =
  let
    numAnimals = length animalDetails
    -- Generate each verse as a string, ensuring it ends with a newline.
    -- `unlines` adds a newline after each line in the list, including the last one.
    allVerses = [unlines (verseLines i) | i <- [0 .. numAnimals - 1]]
  in
    -- Join the verses with a single newline character.
    -- Since each verse string already ends with a newline (from `unlines`),
    -- this results in a blank line between verses (e.g., "line1\nline2\n" + "\n" + "lineA\nlineB\n" -> "line1\nline2\n\nlineA\nlineB\n").
    intercalate "\n" allVerses
