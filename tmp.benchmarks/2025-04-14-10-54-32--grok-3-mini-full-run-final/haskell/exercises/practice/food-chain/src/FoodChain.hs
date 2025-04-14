module FoodChain (song) where

-- List of animals in order
animals :: [String]
animals = ["fly", "spider", "bird", "cat", "dog", "goat", "cow", "horse"]

-- Reason phrases for animals (indexed for spider to cow, i.e., positions 1 to 6)
reasons :: [String]
reasons = [
  "It wriggled and jiggled and tickled inside her.",  -- For spider (index 1)
  "How absurd to swallow a bird!",                    -- For bird (index 2)
  "Imagine that, to swallow a cat!",                  -- For cat (index 3)
  "What a hog, to swallow a dog!",                    -- For dog (index 4)
  "Just opened her throat and swallowed a goat!",     -- For goat (index 5)
  "I don't know how she swallowed a cow!"             -- For cow (index 6)
  ]

-- Function to generate a single verse for the nth animal (0-based index)
verse :: Int -> String
verse n =
  let animal = animals !! n  -- Get the current animal
      intro = "I know an old lady who swallowed a " ++ animal ++ ".\n"
      reasonLine = if n >= 1 && n <= 6  -- Reasons only for indices 1 to 6
                   then reasons !! (n - 1) ++ "\n"  -- Adjust index for reasons list
                   else ""
      cumulative = if n > 0 && n < 7  -- Only for indices 1 to 6
                   then concat [ "She swallowed the " ++ animals !! i ++ " to catch the " ++ animals !! (i - 1) ++
                                 (if i - 1 == 1  -- Special phrase only for spider (index 1)
                                  then " that wriggled and jiggled and tickled inside her.\n"
                                  else ".\n") | i <- [n, n-1 .. 1] ]
                   else ""
      ending = if n < 7  -- For all except horse
               then "I don't know why she swallowed the fly. Perhaps she'll die.\n\n"
               else "She's dead, of course!\n"
  in intro ++ reasonLine ++ cumulative ++ ending  -- Combine parts

-- Main function to generate the full song
song :: String
song = concat [verse i | i <- [0..7]]  -- Generate verses for all 8 animals (0-based)
