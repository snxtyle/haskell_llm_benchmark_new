module FoodChain (song) where

import Data.List (intercalate)

song :: String
song =
  intercalate "\n\n" (map (intercalate "\n" . verseLines) [0 .. length animals - 1])
    ++ "\n"

-- The animals swallowed, in order.
animals :: [String]
animals =
  [ "fly"
  , "spider"
  , "bird"
  , "cat"
  , "dog"
  , "goat"
  , "cow"
  , "horse"
  ]

-- Optional second line per verse (except fly, which has none).
exclamation :: String -> Maybe String
exclamation a =
  case a of
    "fly"    -> Nothing
    "spider" -> Just "It wriggled and jiggled and tickled inside her."
    "bird"   -> Just "How absurd to swallow a bird!"
    "cat"    -> Just "Imagine that, to swallow a cat!"
    "dog"    -> Just "What a hog, to swallow a dog!"
    "goat"   -> Just "Just opened her throat and swallowed a goat!"
    "cow"    -> Just "I don't know how she swallowed a cow!"
    "horse"  -> Just "She's dead, of course!"
    _        -> Nothing

-- Construct the lines for a single verse (by index into 'animals').
verseLines :: Int -> [String]
verseLines i =
  let a = animals !! i
      first = "I know an old lady who swallowed a " ++ a ++ "."
  in
    if a == "horse"
      then [first, "She's dead, of course!"]
      else
        [first]
        ++ maybe [] (:[]) (exclamation a)
        ++ catchLines i
        ++ ["I don't know why she swallowed the fly. Perhaps she'll die."]

-- Build the cumulative catch lines for verse i.
catchLines :: Int -> [String]
catchLines i =
  let preds = reverse (take (i + 1) animals) -- current down to fly
      preys = reverse (take i animals)        -- previous down to fly
  in zipWith catchLine preds preys

catchLine :: String -> String -> String
catchLine predator prey =
  "She swallowed the " ++ predator ++ " to catch the " ++ prey ++ suffix
  where
    suffix
      | prey == "spider" = " that wriggled and jiggled and tickled inside her."
      | otherwise        = "."
