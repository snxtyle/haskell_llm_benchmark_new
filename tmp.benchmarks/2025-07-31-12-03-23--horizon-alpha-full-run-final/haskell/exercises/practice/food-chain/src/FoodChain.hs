module FoodChain (song) where

import Data.List (intercalate)

song :: String
song = intercalate "\n\n" (map verse [0 .. lastIdx])
  where
    lastIdx = length animals - 1

-- Data describing each verse in order.
-- Each entry: (animal, optional unique line after opening)
animals :: [(String, Maybe String)]
animals =
  [ ("fly",    Just "I don't know why she swallowed the fly. Perhaps she'll die.")
  , ("spider", Just "It wriggled and jiggled and tickled inside her.")
  , ("bird",   Just "How absurd to swallow a bird!")
  , ("cat",    Just "Imagine that, to swallow a cat!")
  , ("dog",    Just "What a hog, to swallow a dog!")
  , ("goat",   Just "Just opened her throat and swallowed a goat!")
  , ("cow",    Just "I don't know how she swallowed a cow!")
  , ("horse",  Just "She's dead, of course!")
  ]

verse :: Int -> String
verse i =
  case animal of
    "horse" -> unlines
      [ opening
      , uniq
      ]
    "fly" -> unlines
      [ opening
      , uniq -- fly's concluding line
      ]
    _ ->
      unlines $
        [ opening
        ] ++ maybeToList uniqLine
          ++ cumulativeLines i
          ++ [flyConclusion]
  where
    (animal, uniqLine) = animals !! i
    opening = "I know an old lady who swallowed a " ++ animal ++ "."
    uniq = case uniqLine of
      Just t  -> t
      Nothing -> ""
    flyConclusion = "I don't know why she swallowed the fly. Perhaps she'll die."

-- Build the cumulative "She swallowed the X to catch the Y." lines
-- for a given verse index i, walking backwards until fly.
cumulativeLines :: Int -> [String]
cumulativeLines i =
  [ lineFor a b
  | (a, b) <- pairs
  ]
  where
    -- Pairs computed by indices: i..1 pairing j with j-1
    pairs =
      [ (fst (animals !! j), fst (animals !! (j - 1)))
      | j <- [i, i - 1 .. 1]
      ]

    lineFor :: String -> String -> String
    lineFor a b =
      "She swallowed the " ++ a ++ " to catch the " ++ decorate b ++ "."

    decorate :: String -> String
    decorate "spider" = "spider that wriggled and jiggled and tickled inside her"
    decorate other    = other

-- Helpers

maybeToList :: Maybe String -> [String]
maybeToList (Just x) = [x]
maybeToList Nothing  = []
