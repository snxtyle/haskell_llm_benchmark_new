module FoodChain (song) where

import Data.List  (intercalate)
import Data.Maybe (maybeToList)

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

remark :: String -> Maybe String
remark a = case a of
  "fly"    -> Nothing
  "spider" -> Just "It wriggled and jiggled and tickled inside her."
  "bird"   -> Just "How absurd to swallow a bird!"
  "cat"    -> Just "Imagine that, to swallow a cat!"
  "dog"    -> Just "What a hog, to swallow a dog!"
  "goat"   -> Just "Just opened her throat and swallowed a goat!"
  "cow"    -> Just "I don't know how she swallowed a cow!"
  "horse"  -> Just "She's dead, of course!"
  _        -> Nothing

-- For the spider we need some additional explanatory text that is inserted
-- inside a chaining line.  It must NOT end with a period because the period
-- is added by 'swallowLine'.
extraForSpider :: String -> String
extraForSpider "spider" = " that wriggled and jiggled and tickled inside her"
extraForSpider _        = ""

swallowLine :: String -> String -> String
swallowLine current previous =
  "She swallowed the "
    ++ current
    ++ " to catch the "
    ++ previous
    ++ extraForSpider previous
    ++ "."

verse :: Int -> String
verse idx =
  let a = animals !! idx
      opening = "I know an old lady who swallowed a " ++ a ++ "."
      unique  = maybeToList (remark a)

      chain
        | a == "horse" = []
        | otherwise =
            [ swallowLine (animals !! i) (animals !! (i - 1))
            | i <- [idx, idx - 1 .. 1]
            ]

      closing
        | a == "horse" = []
        | otherwise    = ["I don't know why she swallowed the fly. Perhaps she'll die."]

      linesOfVerse = opening : unique ++ chain ++ closing
  in intercalate "\n" linesOfVerse

song :: String
song = intercalate "\n\n" (map verse [0 .. length animals - 1]) ++ "\n"
