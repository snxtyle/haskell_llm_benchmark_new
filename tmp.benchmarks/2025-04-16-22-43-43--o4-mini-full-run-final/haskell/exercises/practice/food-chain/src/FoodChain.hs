module FoodChain (song, verse, verses) where

import Data.List (intercalate)

animals :: [(String, String)]
animals =
  [ ("fly",    "I don't know why she swallowed the fly. Perhaps she'll die.")
  , ("spider","It wriggled and jiggled and tickled inside her.")
  , ("bird",   "How absurd to swallow a bird!")
  , ("cat",    "Imagine that, to swallow a cat!")
  , ("dog",    "What a hog, to swallow a dog!")
  , ("goat",   "Just opened her throat and swallowed a goat!")
  , ("cow",    "I don't know how she swallowed a cow!")
  , ("horse",  "She's dead, of course!")
  ]

names :: [String]
names = map fst animals

comments :: [String]
comments = map snd animals

spiderSuffix :: String
spiderSuffix = " that wriggled and jiggled and tickled inside her"

-- | Generate a single verse by index (1-based).
verse :: Int -> String
verse i
  | i < 1 || i > length names = error "Verse index out of range"
  | i == 1 =
      unlines
        [ intro
        , comments !! 0
        ]
  | i == length names =
      unlines
        [ intro
        , comments !! (i - 1)
        ]
  | otherwise =
      unlines $
        [ intro
        , comments !! (i - 1)
        ]
          ++ chainLines i
          ++ [comments !! 0]
  where
    intro = "I know an old lady who swallowed a " ++ names !! (i - 1) ++ "."
    chainLines k =
      [ swallowLine j | j <- [k - 1, k - 2 .. 1] ]
    swallowLine j =
      let predator = names !! j
          prey     = names !! (j - 1)
          sfx      = if prey == "spider" then spiderSuffix else ""
       in "She swallowed the " ++ predator ++ " to catch the " ++ prey ++ sfx ++ "."

-- | Generate several verses, inclusive, separated by a blank line.
verses :: Int -> Int -> String
verses m n
  | m > n     = ""
  | otherwise = intercalate "\n" [verse i | i <- [m .. n]]

-- | The full song (verses 1 through 8).
song :: String
song = verses 1 (length names)
