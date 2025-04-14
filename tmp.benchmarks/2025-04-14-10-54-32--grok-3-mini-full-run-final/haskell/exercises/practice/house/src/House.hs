module House (rhyme) where

import Data.List (intercalate)

items :: [String]
items = [
  "the house that Jack built.",
  "the malt",
  "the rat",
  "the cat",
  "the dog",
  "the cow with the crumpled horn",
  "the maiden all forlorn",
  "the man all tattered and torn",
  "the priest all shaven and shorn",
  "the rooster that crowed in the morn",
  "the farmer sowing his corn",
  "the horse and the hound and the horn"
  ]

connectors :: [String]
connectors = [
  "lay in",
  "ate the",
  "killed the",
  "worried the",
  "tossed the",
  "milked the",
  "kissed the",
  "married the",
  "woke the",
  "kept the",
  "belonged to the"
  ]

rhyme :: String
rhyme = unlines (intercalate [""] (map verse [1..12]))

verse :: Int -> String
verse 1 = "This is " ++ items !! 0
verse n = "This is " ++ items !! (n-1) ++ "\n" ++ unlines [ "that " ++ connectors !! k ++ " " ++ items !! k | k <- reverse [0..(n-2)] ]
