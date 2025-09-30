module FoodChain (song) where

import Data.List (intercalate)

data Animal = Animal
  { name :: String
  , reaction :: Maybe String
  , description :: Maybe String
  }

animals :: [Animal]
animals =
  [ Animal "fly" Nothing Nothing
  , Animal "spider" (Just "It wriggled and jiggled and tickled inside her.") (Just "that wriggled and jiggled and tickled inside her")
  , Animal "bird" (Just "How absurd to swallow a bird!") Nothing
  , Animal "cat" (Just "Imagine that, to swallow a cat!") Nothing
  , Animal "dog" (Just "What a hog, to swallow a dog!") Nothing
  , Animal "goat" (Just "Just opened her throat and swallowed a goat!") Nothing
  , Animal "cow" (Just "I don't know how she swallowed a cow!") Nothing
  , Animal "horse" (Just "She's dead, of course!") Nothing
  ]

song :: String
song = intercalate "\n\n" (map makeVerse [0 .. length animals - 1]) ++ "\n"

makeVerse :: Int -> String
makeVerse idx = intercalate "\n" $ filter (not . null) verseLines
  where
    animal = animals !! idx
    verseLines =
      [ "I know an old lady who swallowed a " ++ name animal ++ "."
      , maybe "" id (reaction animal)
      , if name animal == "horse" then "" else cumulativeLines idx
      , if name animal == "horse" then "" else "I don't know why she swallowed the fly. Perhaps she'll die."
      ]

cumulativeLines :: Int -> String
cumulativeLines idx = intercalate "\n" (map makeCatchLine [idx, idx - 1 .. 1])
  where
    makeCatchLine :: Int -> String
    makeCatchLine i =
      let current = animals !! i
          previous = animals !! (i - 1)
          prevDesc = case description previous of
            Just desc -> " " ++ desc
            Nothing -> ""
      in "She swallowed the " ++ name current ++ " to catch the " ++ name previous ++ prevDesc ++ "."
