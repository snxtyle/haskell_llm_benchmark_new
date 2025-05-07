module House (rhyme) where

import Data.List (intercalate)

-- | Lista de sujetos presentados por cada estrofa.
subjects :: [String]
subjects =
  [ "the house that Jack built."
  , "the malt"
  , "the rat"
  , "the cat"
  , "the dog"
  , "the cow with the crumpled horn"
  , "the maiden all forlorn"
  , "the man all tattered and torn"
  , "the priest all shaven and shorn"
  , "the rooster that crowed in the morn"
  , "the farmer sowing his corn"
  , "the horse and the hound and the horn"
  ]

-- | Verbo/acción que conecta cada sujeto con el anterior.
actions :: [String]
actions =
  [ "lay in"
  , "ate"
  , "killed"
  , "worried"
  , "tossed"
  , "milked"
  , "kissed"
  , "married"
  , "woke"
  , "kept"
  , "belonged to"
  ]

-- | Construye una estrofa dada su posición (0‑based).
verse :: Int -> String
verse n =
  unlines $
    header : map tailLine (reverse [0 .. n - 1])
  where
    header    = "This is " ++ subjects !! n
    tailLine k = "that " ++ actions !! k ++ " " ++ subjects !! k

-- | La rima completa, separada por líneas en blanco entre estrofas.
rhyme :: String
rhyme = intercalate "\n" (map verse [0 .. length subjects - 1])
