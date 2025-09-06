module FoodChain (song) where

-- Define the animals in order and their specific lines
data Animal = Animal 
    { name :: String
    , comment :: Maybe String  -- Nothing for fly, Just comment for others
    , isFatal :: Bool          -- True only for horse
    } deriving (Eq, Show)

-- List of animals in swallowing order
animals :: [Animal]
animals = 
    [ Animal "fly" Nothing False
    , Animal "spider" (Just "It wriggled and jiggled and tickled inside her.") False
    , Animal "bird" (Just "How absurd to swallow a bird!") False
    , Animal "cat" (Just "Imagine that, to swallow a cat!") False
    , Animal "dog" (Just "What a hog, to swallow a dog!") False
    , Animal "goat" (Just "Just opened her throat and swallowed a goat!") False
    , Animal "cow" (Just "I don't know how she swallowed a cow!") False
    , Animal "horse" Nothing True
    ]

-- Generate a single verse for an animal
verse :: Int -> String
verse i = 
    let animal = animals !! i
        firstLine = "I know an old lady who swallowed a " ++ name animal ++ ".\n"
        commentLine = case comment animal of
                        Just c -> c ++ "\n"
                        Nothing -> ""
        middleLines = if isFatal animal
                      then ""
                      else generateMiddleLines i
        lastLine = if isFatal animal
                   then "She's dead, of course!\n"
                   else "I don't know why she swallowed the fly. Perhaps she'll die.\n"
    in firstLine ++ commentLine ++ middleLines ++ lastLine

-- Generate the middle lines (she swallowed X to catch Y) for a given animal index
generateMiddleLines :: Int -> String
generateMiddleLines 0 = ""  -- Fly has no middle lines
generateMiddleLines i = 
    let currentAnimals = take (i + 1) animals
        -- Create pairs of consecutive animals: (bird, spider), (spider, fly), etc.
        animalPairs = zip currentAnimals (tail currentAnimals)
        catchLines = map (\(predator, prey) -> 
            let target = if name prey == "spider" 
                         then "spider that wriggled and jiggled and tickled inside her"
                         else name prey
            in "She swallowed the " ++ name predator ++ " to catch the " ++ target ++ ".\n")
            animalPairs
    in concat catchLines

-- Generate the complete song
song :: String
song = concatMap (\i -> verse i ++ (if i < length animals - 1 then "\n" else "")) [0..length animals - 1]
