module House (rhyme) where

subjects :: [String]
subjects = [
    "the house that Jack built",
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

actions :: [String]
actions = [
    "lay in",
    "ate",
    "killed",
    "worried",
    "tossed",
    "milked",
    "kissed",
    "married",
    "woke",
    "kept",
    "belonged to"
    ]

verse :: Int -> String
verse n = "This is " ++ subjects !! n ++ (if n == 0 then ".\n\n" else "\n") ++
          concat [ "that " ++ actions !! i ++ " " ++ subjects !! i ++
                   (if i == 0 then ".\n" ++ (if n == 11 then "" else "\n") else "\n")
                 | i <- [n-1, n-2 .. 0] ]

rhyme :: String
rhyme = concat [ verse n | n <- [0..11] ]
