module FoodChain (song) where

import Data.List (intercalate)

-- Data structure to hold information about each animal
data Animal = Animal
    { name :: String
    , reaction :: String
    , action :: String
    }

-- Define all the animals and their associated text
animals :: [Animal]
animals =
    [ Animal "fly" "" ""
    , Animal "spider" "It wriggled and jiggled and tickled inside her." "wriggled and jiggled and tickled inside her"
    , Animal "bird" "How absurd to swallow a bird!" ""
    , Animal "cat" "Imagine that, to swallow a cat!" ""
    , Animal "dog" "What a hog, to swallow a dog!" ""
    , Animal "goat" "Just opened her throat and swallowed a goat!" ""
    , Animal "cow" "I don't know how she swallowed a cow!" ""
    , Animal "horse" "She's dead, of course!" ""
    ]

-- Generate the opening line for each verse
openingLine :: Animal -> String
openingLine animal = "I know an old lady who swallowed a " ++ name animal ++ "."

-- Generate the reaction line if it exists
reactionLine :: Animal -> Maybe String
reactionLine animal
    | null (reaction animal) = Nothing
    | otherwise = Just (reaction animal)

-- Generate the chain of "swallowed X to catch Y" lines
swallowedChain :: Int -> [String]
swallowedChain verseNum
    | verseNum <= 1 = []
    | otherwise = buildChain verseNum
  where
    buildChain n
        | n <= 1 = []
        | otherwise = swallowedLine n : buildChain (n - 1)
    
    swallowedLine idx =
        let current = animals !! (idx - 1)
            previous = animals !! (idx - 2)
            previousName = if name previous == "spider" && not (null (action previous))
                          then name previous ++ " that " ++ action previous
                          else name previous
        in "She swallowed the " ++ name current ++ " to catch the " ++ previousName ++ "."

-- Generate the ending refrain
endingRefrain :: String
endingRefrain = "I don't know why she swallowed the fly. Perhaps she'll die."

-- Generate a single verse
generateVerse :: Int -> String
generateVerse verseNum =
    let animal = animals !! (verseNum - 1)
        verseLines = filter (not . null)
            [ openingLine animal
            , maybe "" id (reactionLine animal)
            , intercalate "\n" (swallowedChain verseNum)
            , if verseNum == 8 then "" else endingRefrain
            ]
    in intercalate "\n" verseLines

-- Generate the complete song
song :: String
song = intercalate "\n\n" (map generateVerse [1..8]) ++ "\n"
