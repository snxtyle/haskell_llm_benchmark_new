module FoodChain (song) where

import Data.List (intersperse)

song :: String
song = unlines $ concat $ intersperse [""] (map verse [0 .. lastIndex])
  where
    lastIndex = length animals - 1

verse :: Int -> [String]
verse i
  | i == lastIndex = [intro name, finalRemark]
  | otherwise = [intro name] ++ remarkLines ++ chainLines ++ [ending]
  where
    lastIndex = length animals - 1
    (name, remark) = animals !! i

    intro n = "I know an old lady who swallowed a " ++ n ++ "."

    remarkLines = maybe [] (: []) remark

    chainLines =
      [ "She swallowed the " ++ animalName j
        ++ " to catch the " ++ target (j - 1) ++ "."
      | j <- [i, i - 1 .. 1]
      ]

    ending = "I don't know why she swallowed the fly. Perhaps she'll die."

    finalRemark = maybe "" id remark

    animalName j = fst (animals !! j)

    target j =
      let n = fst (animals !! j)
       in if n == "spider"
            then "spider that wriggled and jiggled and tickled inside her"
            else n

animals :: [(String, Maybe String)]
animals =
  [ ("fly", Nothing)
  , ("spider", Just "It wriggled and jiggled and tickled inside her.")
  , ("bird", Just "How absurd to swallow a bird!")
  , ("cat", Just "Imagine that, to swallow a cat!")
  , ("dog", Just "What a hog, to swallow a dog!")
  , ("goat", Just "Just opened her throat and swallowed a goat!")
  , ("cow", Just "I don't know how she swallowed a cow!")
  , ("horse", Just "She's dead, of course!")
  ]
