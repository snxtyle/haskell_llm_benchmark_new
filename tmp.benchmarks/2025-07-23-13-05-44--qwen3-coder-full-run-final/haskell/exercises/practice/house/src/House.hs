module House (rhyme) where

rhyme :: String
rhyme = unlines $ map makeVerse [0..11]

makeVerse :: Int -> String
makeVerse n = "This is " ++ makeChain n

makeChain :: Int -> String
makeChain 0 = "the house that Jack built."
makeChain n = (subjects !! n) ++ "\n" ++ (actions !! (n - 1)) ++ makeChain (n - 1)

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

actions :: [String]
actions = 
  [ "that lay in "
  , "that ate "
  , "that killed "
  , "that worried "
  , "that tossed "
  , "that milked "
  , "that kissed "
  , "that married "
  , "that woke "
  , "that kept "
  , "that belonged to "
  ]
