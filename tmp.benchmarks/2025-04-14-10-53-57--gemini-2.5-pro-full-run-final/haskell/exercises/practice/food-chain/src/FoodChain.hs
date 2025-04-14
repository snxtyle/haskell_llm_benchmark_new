module FoodChain (song) where

import Data.List (intercalate)

-- Define a type for the animals and their specific remarks
data AnimalInfo = AnimalInfo { animalName :: String, animalRemark :: Maybe String }

-- List of animals and their remarks, in order they appear in the song
animals :: [AnimalInfo]
animals =
  [ AnimalInfo "fly"    Nothing
  , AnimalInfo "spider" (Just "It wriggled and jiggled and tickled inside her.")
  , AnimalInfo "bird"   (Just "How absurd to swallow a bird!")
  , AnimalInfo "cat"    (Just "Imagine that, to swallow a cat!")
  , AnimalInfo "dog"    (Just "What a hog, to swallow a dog!")
  , AnimalInfo "goat"   (Just "Just opened her throat and swallowed a goat!")
  , AnimalInfo "cow"    (Just "I don't know how she swallowed a cow!")
  , AnimalInfo "horse"  Nothing -- Horse remark is handled specially in the last verse
  ]

-- Generates the lines explaining why the current animal was swallowed
-- by chaining down to the previous animals.
swallowChain :: Int -> [String]
swallowChain n
  | n <= 0    = [] -- Base case: fly has no preceding animal to catch
  | otherwise = map buildLine [n, n-1 .. 1]
  where
    buildLine i =
      let predator = animalName (animals !! i)
          preyInfo = animals !! (i - 1)
          preyName = animalName preyInfo
          -- Special text added only when catching the spider
          spiderExtra = if preyName == "spider"
                          then " that wriggled and jiggled and tickled inside her"
                          else ""
      in "She swallowed the " ++ predator ++ " to catch the " ++ preyName ++ spiderExtra ++ "."

-- Generates a single verse of the song based on the animal's index
verse :: Int -> String
verse n
  | n < 0 || n >= length animals = "" -- Safety check for invalid index
  | n == length animals - 1 = -- Special case for the last verse (horse)
      "I know an old lady who swallowed a horse.\n" ++
      "She's dead, of course!\n"
  | otherwise = -- Standard verse structure
      let current = animals !! n
          intro = "I know an old lady who swallowed a " ++ animalName current ++ ".\n"
          -- Add the unique remark if it exists for the current animal
          remarkLine = case animalRemark current of
                         Just r  -> r ++ "\n"
                         Nothing -> "" -- Fly doesn't have a unique second line here
          -- Generate the chain of swallowing actions
          chainLines = unlines $ swallowChain n
          -- Standard ending for all verses except the last one
          outro = "I don't know why she swallowed the fly. Perhaps she'll die.\n"
      in intro ++ remarkLine ++ chainLines ++ outro

-- Generates the full song by creating each verse and joining them.
song :: String
song = intercalate "\n" $ map verse [0 .. length animals - 1]
