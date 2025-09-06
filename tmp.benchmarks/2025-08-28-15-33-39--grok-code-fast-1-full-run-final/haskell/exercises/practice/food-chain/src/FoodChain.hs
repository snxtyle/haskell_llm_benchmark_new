module FoodChain (song) where

import Data.List (intersperse)

data Animal = Animal { name :: String, intro :: String, chainExtra :: Maybe String }

animals :: [Animal]
animals =
  [ Animal "fly" "" Nothing,
    Animal "spider" "It wriggled and jiggled and tickled inside her." (Just "wriggled and jiggled and tickled inside her."),
    Animal "bird" "How absurd to swallow a bird!" Nothing,
    Animal "cat" "Imagine that, to swallow a cat!" Nothing,
    Animal "dog" "What a hog, to swallow a dog!" Nothing,
    Animal "goat" "Just opened her throat and swallowed a goat!" Nothing,
    Animal "cow" "I don't know how she swallowed a cow!" Nothing,
    Animal "horse" "She's dead, of course!" Nothing
  ]

song :: String
song = concat $ intersperse "\n" $ map verse [0..7]

verse :: Int -> String
verse i = let a = animals !! i
              verseLines = ["I know an old lady who swallowed a " ++ name a ++ "."] ++
                           (if null (intro a) then [] else [intro a]) ++
                           chainLines i ++
                           (if i == 7 then [] else ["I don't know why she swallowed the fly. Perhaps she'll die."])
          in unlines verseLines

chainLines :: Int -> [String]
chainLines i
  | i == 7 = []
  | otherwise = [ "She swallowed the " ++ name (animals !! k) ++ " to catch the " ++ name (animals !! (k-1)) ++
                  (case chainExtra (animals !! (k-1)) of
                     Just extra -> " that " ++ extra
                     Nothing -> "") ++ "."
                | k <- [i, i-1 .. 1] ]
