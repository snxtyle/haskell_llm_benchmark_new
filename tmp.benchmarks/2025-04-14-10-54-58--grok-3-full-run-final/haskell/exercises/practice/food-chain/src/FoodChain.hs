module FoodChain (song) where

import Data.List (intersperse)

-- Data structure to hold information about each animal
data Animal = Animal {
    name :: String,
    remark :: String,
    reference :: String
}

-- List of animals in the order they are swallowed
animals :: [Animal]
animals = [
    Animal "fly" "" "fly",
    Animal "spider" "It wriggled and jiggled and tickled inside her." "spider that wriggled and jiggled and tickled inside her",
    Animal "bird" "How absurd to swallow a bird!" "bird",
    Animal "cat" "Imagine that, to swallow a cat!" "cat",
    Animal "dog" "What a hog, to swallow a dog!" "dog",
    Animal "goat" "Just opened her throat and swallowed a goat!" "goat",
    Animal "cow" "I don't know how she swallowed a cow!" "cow",
    Animal "horse" "She's dead, of course!" "horse"
    ]

-- Generate a single verse for the given animal index
generateVerse :: Int -> String
generateVerse index = 
    let currentAnimal = animals !! index
        intro = "I know an old lady who swallowed a " ++ name currentAnimal ++ ".\n"
        remarkLine = if null (remark currentAnimal) then "" else remark currentAnimal ++ "\n"
        chain = if index == length animals - 1 then "" else generateChain index
        ending = if index == length animals - 1 then "" else "I don't know why she swallowed the fly. Perhaps she'll die.\n"
    in intro ++ remarkLine ++ chain ++ ending

-- Generate the cumulative chain of "swallowed to catch" lines
generateChain :: Int -> String
generateChain index = 
    let chainAnimals = reverse $ take index animals
        lines = zipWith (\a b -> "She swallowed the " ++ name a ++ " to catch the " ++ reference b ++ ".\n") chainAnimals (tail chainAnimals ++ [head animals])
    in concat lines

-- Generate the complete song
song :: String
song = 
    let verses = map generateVerse [0..length animals - 1]
    in concat $ intersperse "\n" verses
