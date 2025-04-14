module FoodChain (song) where

import Data.List (intercalate)

data Animal = Animal
    { name :: String
    , remark :: Maybe String
    , catchPhrase :: Maybe String
    }

animals :: [Animal]
animals =
    [ Animal "fly"    Nothing Nothing
    , Animal "spider" (Just "It wriggled and jiggled and tickled inside her.") (Just "spider that wriggled and jiggled and tickled inside her")
    , Animal "bird"   (Just "How absurd to swallow a bird!") (Just "bird")
    , Animal "cat"    (Just "Imagine that, to swallow a cat!") (Just "cat")
    , Animal "dog"    (Just "What a hog, to swallow a dog!") (Just "dog")
    , Animal "goat"   (Just "Just opened her throat and swallowed a goat!") (Just "goat")
    , Animal "cow"    (Just "I don't know how she swallowed a cow!") (Just "cow")
    , Animal "horse"  (Just "She's dead, of course!") Nothing
    ]

song :: String
song = intercalate "\n\n" $ map verse [0..length animals - 1]

verse :: Int -> String
verse n
    | n == length animals - 1 = lastVerse
    | otherwise = intercalate "\n" $ filter (not . null)
        [ firstLine
        , maybe "" id (remark animal)
        , swallowLines
        , ending
        ]
  where
    animal = animals !! n
    firstLine = "I know an old lady who swallowed a " ++ name animal ++ "."
    swallowLines = intercalate "\n" $ map swallowLine (reverse [1..n])
    swallowLine i =
        let curr = animals !! i
            prev = animals !! (i - 1)
            phrase = case catchPhrase prev of
                Just _ | name prev == "spider" -> "spider that wriggled and jiggled and tickled inside her"
                Just p -> p
                Nothing -> name prev
        in "She swallowed the " ++ name curr ++ " to catch the " ++ phrase ++ "."
    ending = if n == 0 then "I don't know why she swallowed the fly. Perhaps she'll die." else "I don't know why she swallowed the fly. Perhaps she'll die."

lastVerse :: String
lastVerse = "I know an old lady who swallowed a horse.\nShe's dead, of course!"
