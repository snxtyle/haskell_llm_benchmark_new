module FoodChain (song) where

import Data.List  (intercalate)
import Data.Maybe (maybeToList)

data Animal = Animal
    { name          :: String   -- animal’s name
    , remark        :: Maybe String -- optional, verse–specific remark
    , preyQualifier :: String   -- how this animal is referred to when it is prey
    }

-- All the animals that the old lady swallows, in order.
animals :: [Animal]
animals =
    [ Animal "fly"    Nothing                                      "fly"
    , Animal "spider" (Just "It wriggled and jiggled and tickled inside her.") "spider that wriggled and jiggled and tickled inside her"
    , Animal "bird"   (Just "How absurd to swallow a bird!")       "bird"
    , Animal "cat"    (Just "Imagine that, to swallow a cat!")     "cat"
    , Animal "dog"    (Just "What a hog, to swallow a dog!")       "dog"
    , Animal "goat"   (Just "Just opened her throat and swallowed a goat!") "goat"
    , Animal "cow"    (Just "I don't know how she swallowed a cow!") "cow"
    , Animal "horse"  (Just "She's dead, of course!")              "horse"
    ]

-- The final line for every verse except the last.
concluding :: String
concluding = "I don't know why she swallowed the fly. Perhaps she'll die."

-- Produce a single verse (0‑based index).
verse :: Int -> String
verse n =
    let a     = animals !! n
        intro = "I know an old lady who swallowed a " ++ name a ++ "."
    in case name a of
         "horse" -> unlines [intro, maybe "" id (remark a)]
         "fly"   -> unlines [intro, concluding]
         _       -> unlines $
                      [intro]
                      ++ maybeToList (remark a)
                      ++ chainLines n
                      ++ [concluding]

-- Generate the chain of ‘She swallowed the X to catch the Y.’ lines
-- for verse n, counting down to the fly.
chainLines :: Int -> [String]
chainLines n =
    [ "She swallowed the " ++ name predator
      ++ " to catch the " ++ preyQualifier prey ++ "."
    | k <- [n, n - 1 .. 1]
    , let predator = animals !! k
          prey     = animals !! (k - 1)
    ]

-- The complete song, verses separated by a blank line.
song :: String
song = intercalate "\n" (map verse [0 .. length animals - 1])
