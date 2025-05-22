module FoodChain (song) where

import Data.List (intercalate)

data Animal = Animal
  { name :: String
  , reaction :: String
  , special :: Maybe String
  } deriving (Show)

animals :: [Animal]
animals = 
  [ Animal "fly" "" Nothing
  , Animal "spider" "It wriggled and jiggled and tickled inside her." (Just "that wriggled and jiggled and tickled inside her")
  , Animal "bird" "How absurd to swallow a bird!" Nothing
  , Animal "cat" "Imagine that, to swallow a cat!" Nothing
  , Animal "dog" "What a hog, to swallow a dog!" Nothing
  , Animal "goat" "Just opened her throat and swallowed a goat!" Nothing
  , Animal "cow" "I don't know how she swallowed a cow!" Nothing
  , Animal "horse" "She's dead, of course!" Nothing
  ]

openingLine :: String -> String
openingLine animal = "I know an old lady who swallowed a " ++ animal ++ "."

reactionLine :: String -> String
reactionLine "" = ""
reactionLine reaction = reaction

catchLine :: Animal -> Animal -> String
catchLine current previous = 
  "She swallowed the " ++ name current ++ " to catch the " ++ name previous ++ 
  case special previous of
    Just spec -> " " ++ spec ++ "."
    Nothing -> "."

finalLine :: String
finalLine = "I don't know why she swallowed the fly. Perhaps she'll die."

generateVerse :: Int -> String
generateVerse index
  | index >= length animals = ""
  | name currentAnimal == "horse" = 
      openingLine (name currentAnimal) ++ "\n" ++ reaction currentAnimal
  | otherwise = 
      let opening = openingLine (name currentAnimal)
          reactionPart = if null (reaction currentAnimal) then "" else reaction currentAnimal ++ "\n"
          catchLines = if index == 0 
                      then ""
                      else intercalate "\n" (generateCatchLines index) ++ "\n"
          ending = finalLine
      in opening ++ "\n" ++ reactionPart ++ catchLines ++ ending
  where
    currentAnimal = animals !! index

generateCatchLines :: Int -> [String]
generateCatchLines index = 
  [ catchLine (animals !! i) (animals !! (i - 1)) | i <- [index, index - 1 .. 1] ]

song :: String
song = intercalate "\n\n" [generateVerse i | i <- [0 .. length animals - 1]] ++ "\n"
