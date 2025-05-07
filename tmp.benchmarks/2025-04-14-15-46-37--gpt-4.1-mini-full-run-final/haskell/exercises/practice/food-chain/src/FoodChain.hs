module FoodChain (song) where

data Animal = Animal
  { name :: String
  , comment :: String
  , catchPhrase :: Maybe String
  }

animals :: [Animal]
animals =
  [ Animal "fly" "" Nothing
  , Animal "spider" "It wriggled and jiggled and tickled inside her." (Just "She swallowed the spider to catch the fly.")
  , Animal "bird" "How absurd to swallow a bird!" (Just "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her.")
  , Animal "cat" "Imagine that, to swallow a cat!" (Just "She swallowed the cat to catch the bird.")
  , Animal "dog" "What a hog, to swallow a dog!" (Just "She swallowed the dog to catch the cat.")
  , Animal "goat" "Just opened her throat and swallowed a goat!" (Just "She swallowed the goat to catch the dog.")
  , Animal "cow" "I don't know how she swallowed a cow!" (Just "She swallowed the cow to catch the goat.")
  , Animal "horse" "She's dead, of course!" Nothing
  ]

introLine :: String -> String
introLine animalName = "I know an old lady who swallowed a " ++ animalName ++ "."

lastLine :: String
lastLine = "I don't know why she swallowed the fly. Perhaps she'll die."

buildVerse :: Int -> String
buildVerse n
  | n < 0 || n >= length animals = ""
  | name currentAnimal == "horse" = unlines [introLine (name currentAnimal), comment currentAnimal]
  | otherwise = unlines $ intro : commentLine ++ catchLines ++ [lastLine, ""]
  where
    currentAnimal = animals !! n
    intro = introLine (name currentAnimal)
    commentLine = if null (comment currentAnimal) then [] else [comment currentAnimal]
    catchLines = reverse $ buildCatchLines n

buildCatchLines :: Int -> [String]
buildCatchLines n
  | n <= 0 = []
  | otherwise = case catchPhrase (animals !! i) of
      Just phrase -> phrase : buildCatchLines (i - 1)
      Nothing -> buildCatchLines (i - 1)
  where
    i = n - 1

song :: String
song = concatMap buildVerse [0 .. length animals - 1]
