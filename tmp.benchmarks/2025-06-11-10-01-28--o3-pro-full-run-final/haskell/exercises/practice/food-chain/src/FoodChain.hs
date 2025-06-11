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

commentFor :: String -> Maybe String
commentFor "fly"    = Nothing
commentFor "spider" = Just "It wriggled and jiggled and tickled inside her."
commentFor "bird"   = Just "How absurd to swallow a bird!"
commentFor "cat"    = Just "Imagine that, to swallow a cat!"
commentFor "dog"    = Just "What a hog, to swallow a dog!"
commentFor "goat"   = Just "Just opened her throat and swallowed a goat!"
commentFor "cow"    = Just "I don't know how she swallowed a cow!"
commentFor "horse"  = Just "She's dead, of course!"
commentFor _        = Nothing   -- Fallback (shouldn't happen)

refrain :: String
refrain = "I don't know why she swallowed the fly. Perhaps she'll die."

swallowLine :: String -> String -> String
swallowLine predator prey =
    "She swallowed the " ++ predator ++ " to catch the " ++ prey ++ descriptor ++ "."
  where
    descriptor
      | prey == "spider" = " that wriggled and jiggled and tickled inside her"
      | otherwise        = ""

verseLines :: Int -> [String]
verseLines n =
  let animal       = animals !! n
      opening      = "I know an old lady who swallowed a " ++ animal ++ "."
      commentLine  = maybeToList (commentFor animal)
      cumulative
        | animal == "horse" = []          -- Last verse: no cumulative lines, no refrain
        | otherwise         = chain ++ [refrain]
        where
          chain = [ swallowLine (animals !! k) (animals !! (k - 1))
                  | k <- [n, n - 1 .. 1]
                  ]
  in  [opening] ++ commentLine ++ cumulative

verses :: [String]
verses = map (unlines . verseLines) [0 .. length animals - 1]

-- | The complete song, ending with a newline.
song :: String
song = intercalate "\n" verses
