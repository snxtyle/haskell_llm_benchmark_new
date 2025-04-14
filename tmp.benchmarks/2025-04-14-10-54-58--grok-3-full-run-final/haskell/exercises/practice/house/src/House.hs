module House (rhyme) where

rhyme :: String
rhyme = unlines $ concatMap buildStanza [0..11]
  where
    elements = [
        ("the house", "that Jack built"),
        ("the malt", "that lay in"),
        ("the rat", "that ate"),
        ("the cat", "that killed"),
        ("the dog", "that worried"),
        ("the cow with the crumpled horn", "that tossed"),
        ("the maiden all forlorn", "that milked"),
        ("the man all tattered and torn", "that kissed"),
        ("the priest all shaven and shorn", "that married"),
        ("the rooster that crowed in the morn", "that woke"),
        ("the farmer sowing his corn", "that kept"),
        ("the horse and the hound and the horn", "that belonged to")
      ]
    
    buildStanza n = ["This is " ++ fst (elements !! n) ++ (if n == 0 then " " ++ snd (elements !! 0) else "")] ++
                    map (\i -> indent ++ snd (elements !! i) ++ " " ++ fst (elements !! i)) (reverse [0..n-1]) ++
                    if n == 0 then [] else [indent ++ snd (elements !! 0) ++ "."]
      where indent = replicate 4 ' '
