module FoodChain (song) where

song :: String
song = unlines $ zipWith verse [1..8] creatures

data Creature = Creature 
  { name :: String
  , reaction :: String
  , catchPhrase :: String
  }

creatures :: [Creature]
creatures = 
  [ Creature "fly" "" ""
  , Creature "spider" "It wriggled and jiggled and tickled inside her." "spider that wriggled and jiggled and tickled inside her"
  , Creature "bird" "How absurd to swallow a bird!" "bird"
  , Creature "cat" "Imagine that, to swallow a cat!" "cat"
  , Creature "dog" "What a hog, to swallow a dog!" "dog"
  , Creature "goat" "Just opened her throat and swallowed a goat!" "goat"
  , Creature "cow" "I don't know how she swallowed a cow!" "cow"
  , Creature "horse" "" "horse"
  ]

verse :: Int -> Creature -> String
verse n creature = unlines $ 
  [ intro (name creature)
  , reaction creature
  ] ++ 
  (if n == 8 
   then [] 
   else if n == 1 
        then [outro]
        else catchLines n ++ [outro])

intro :: String -> String
intro animal = "I know an old lady who swallowed a " ++ animal ++ "."

outro :: String
outro = "I don't know why she swallowed the fly. Perhaps she'll die."

catchLines :: Int -> [String]
catchLines n = map makeCatchLine [(n-1),(n-2)..1]
  where
    makeCatchLine i = 
      let predator = name (creatures !! (i-1))
          prey = if i > 1 
                 then catchPhrase (creatures !! (i-2))
                 else name (creatures !! (i-2))
      in "She swallowed the " ++ predator ++ " to catch the " ++ prey ++ "."

