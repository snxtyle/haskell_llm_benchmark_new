module FoodChain (song) where

import Data.List (intercalate)

data Animal = Animal
    { name :: String
    , reaction :: String
    , special :: Maybe String
    }

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

song :: String
song = intercalate "\n" $ map verse [1..length animals]

verse :: Int -> String
verse n
    | n == length animals = lastVerse
    | otherwise = normalVerse n

normalVerse :: Int -> String
normalVerse n = unlines $
    [ "I know an old lady who swallowed a " ++ name (animals !! (n - 1)) ++ "." ] ++
    reactionLine (n - 1) ++
    swallowChain n ++
    [ "I don't know why she swallowed the fly. Perhaps she'll die." ]

lastVerse :: String
lastVerse = 
    "I know an old lady who swallowed a horse.\n" ++
    "She's dead, of course!"

reactionLine :: Int -> [String]
reactionLine 0 = []
reactionLine i = [reaction (animals !! i)]

swallowChain :: Int -> [String]
swallowChain n = map swallowLine [n-1, n-2 .. 1]

swallowLine :: Int -> String
swallowLine i = 
    "She swallowed the " ++ name (animals !! i) ++ 
    " to catch the " ++ preyDescription i ++ "."

preyDescription :: Int -> String
preyDescription i = 
    let prey = animals !! (i - 1)
    in case special prey of
        Just desc -> name prey ++ " " ++ desc
        Nothing -> name prey
