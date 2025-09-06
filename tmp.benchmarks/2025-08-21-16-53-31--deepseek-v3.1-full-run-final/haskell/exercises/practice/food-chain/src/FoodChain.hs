module FoodChain (song) where

import Data.List (intercalate)

data Animal = Animal
    { name :: String
    , comment :: String
    }

animals :: [Animal]
animals =
    [ Animal "fly"    ""
    , Animal "spider" "It wriggled and jiggled and tickled inside her."
    , Animal "bird"   "How absurd to swallow a bird!"
    , Animal "cat"    "Imagine that, to swallow a cat!"
    , Animal "dog"    "What a hog, to swallow a dog!"
    , Animal "goat"   "Just opened her throat and swallowed a goat!"
    , Animal "cow"    "I don't know how she swallowed a cow!"
    , Animal "horse"  "She's dead, of course!"
    ]

verse :: Int -> String
verse n
    | n == 8 = -- Horse verse (special case)
        "I know an old lady who swallowed a horse.\n" ++
        comment (animals !! 7)
    | otherwise =
        let current = animals !! (n - 1)
            intro = "I know an old lady who swallowed a " ++ name current ++ ".\n"
            specComment = if null (comment current) then "" else comment current ++ "\n"
            chain = if n > 1 then intercalate "\n" (map catchLine [n, n-1 .. 2]) ++ "\n" else ""
            ending = "I don't know why she swallowed the fly. Perhaps she'll die."
        in intro ++ specComment ++ chain ++ ending
  where
    catchLine i
        | i == 2 = "She swallowed the spider to catch the fly."
        | otherwise =
            let predator = animals !! (i - 1)
                prey = animals !! (i - 2)
                preyDesc = if i - 1 == 2  -- Special case for spider
                    then name prey ++ " that wriggled and jiggled and tickled inside her"
                    else name prey
            in "She swallowed the " ++ name predator ++ " to catch the " ++ preyDesc ++ "."

song :: String
song = intercalate "\n\n" $ map verse [1..8]
