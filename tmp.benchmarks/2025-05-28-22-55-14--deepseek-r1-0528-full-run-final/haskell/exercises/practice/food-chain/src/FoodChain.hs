module FoodChain (song) where

import Data.List (intersperse)

song :: String
song = unlines $ concat $ intersperse [""] verses
  where
    animals = ["fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"]
    comments = 
      [ "It wriggled and jiggled and tickled inside her."
      , "How absurd to swallow a bird!"
      , "Imagine that, to swallow a cat!"
      , "What a hog, to swallow a dog!"
      , "Just opened her throat and swallowed a goat!"
      , "I don't know how she swallowed a cow!"
      ]
    chainDescription "spider" = "spider that wriggled and jiggled and tickled inside her"
    chainDescription other = other
    verse 0 = 
      [ "I know an old lady who swallowed a fly."
      , "I don't know why she swallowed the fly. Perhaps she'll die."
      ]
    verse 7 = 
      [ "I know an old lady who swallowed a horse."
      , "She's dead, of course!"
      ]
    verse n = 
      let animal = animals !! n
          comment = comments !! (n-1)
          chainLines = [ "She swallowed the " ++ (animals !! j) ++ " to catch the " ++ chainDescription (animals !! (j-1)) ++ "." | j <- [n, n-1..1] ]
      in [ "I know an old lady who swallowed a " ++ animal ++ "."
         , comment
         ] ++ chainLines ++ [ "I don't know why she swallowed the fly. Perhaps she'll die." ]
    verses = map verse [0..7]
