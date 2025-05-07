module House (rhyme) where

import Data.List (intercalate)

-- Each element in 'phrases' is a pair: (subject, action).
-- 'subject' is the main noun phrase of a line (e.g., "the house that Jack built", "the malt").
-- 'action' is the phrase that links the current subject to the *previous* subject in the rhyme's chain
-- (e.g., "that lay in", "that ate").
-- The first phrase is the base of the rhyme; its 'action' component is not used for linking.
phrases :: [(String, String)]
phrases = [
    ("the house that Jack built.", "") 
  , ("the malt", "that lay in")
  , ("the rat", "that ate")
  , ("the cat", "that killed")
  , ("the dog", "that worried")
  , ("the cow with the crumpled horn", "that tossed")
  , ("the maiden all forlorn", "that milked")
  , ("the man all tattered and torn", "that kissed")
  , ("the priest all shaven and shorn", "that married")
  , ("the rooster that crowed in the morn", "that woke")
  , ("the farmer sowing his corn", "that kept")
  , ("the horse and the hound and the horn", "that belonged to")
  ]

-- Generates the lines for a single verse of the rhyme.
-- The 'verseIndex' corresponds to the main subject of the verse.
-- For example, verseIndex 0 is for "the house...", verseIndex 1 for "the malt...".
verseLines :: Int -> [String]
verseLines verseIndex =
    let (currentSubject, _) = phrases !! verseIndex
        firstLine = "This is " ++ currentSubject
        
        -- Recursively builds the chain of "that ..." lines for the current verse.
        -- 'currentIndex' refers to the index in 'phrases' for the current part of the chain.
        buildChain :: Int -> [String]
        buildChain currentIndex
            -- Base case: If we're at the first phrase (the house), the chain ends.
            | currentIndex == 0 = []
            -- Recursive step: Construct the line linking the current phrase to the previous one.
            | otherwise         =
                let (prevSubject, _)   = phrases !! (currentIndex - 1) -- Subject of the previous phrase in the chain
                    (_, currentAction) = phrases !! currentIndex     -- Action of the current phrase
                in (currentAction ++ " " ++ prevSubject) : buildChain (currentIndex - 1)
                
    in firstLine : buildChain verseIndex

-- Constructs the full nursery rhyme.
rhyme :: String
rhyme =
    let numPhrases = length phrases
        -- Generate each verse. 'unlines' joins lines with '\n' and adds a trailing '\n'.
        allVerseStrings = [unlines (verseLines i) | i <- [0 .. numPhrases - 1]]
    -- Join all verses. Since each verse string already ends with '\n', 
    -- intercalating with "\n" results in two newlines (a blank line) between verses.
    in intercalate "\n" allVerseStrings
