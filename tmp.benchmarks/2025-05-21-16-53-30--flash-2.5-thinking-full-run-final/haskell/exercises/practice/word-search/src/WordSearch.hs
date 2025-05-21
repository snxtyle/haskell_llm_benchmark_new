module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

-- All 8 possible directions (dr, dc)
-- (delta row, delta column)
directions :: [(Int, Int)]
directions =
    [ (-1, -1) -- Up-Left
    , (-1, 0)  -- Up
    , (-1, 1)  -- Up-Right
    , (0, -1)  -- Left
    , (0, 1)   -- Right
    , (1, -1)  -- Down-Left
    , (1, 0)   -- Down
    , (1, 1)   -- Down-Right
    ]

-- Get the character at a specific CharPos, if it's within bounds
getCharFromGrid :: [String] -> CharPos -> Maybe Char
getCharFromGrid grid (CharPos c r)
    | r >= 0 && r < numRows && c >= 0 && c < numCols = Just (grid !! r !! c)
    | otherwise = Nothing
  where
    numRows = length grid
    numCols = if numRows > 0 then length (head grid) else 0

-- Check if a CharPos is within the grid bounds
isValidPos :: [String] -> CharPos -> Bool
isValidPos grid (CharPos c r) =
    r >= 0 && r < numRows && c >= 0 && c < numCols
  where
    numRows = length grid
    numCols = if numRows > 0 then length (head grid) else 0

-- Try to find a word starting from a given position and moving in a specific direction
findWordFromPosAndDir :: [String] -> String -> CharPos -> (Int, Int) -> Maybe WordPos
findWordFromPosAndDir grid word startPos (dr, dc) =
    if null word then Nothing -- An empty word cannot be found
    else checkPath startPos 0 startPos -- Pass current char pos, char index, and last matched char pos
  where
    wordLen = length word

    -- Recursive helper to check characters along the path
    -- currentPos: The position of the character we are currently trying to match
    -- charIdx: The index of the character in 'word' we are trying to match
    -- lastMatchedPos: The position of the last character that successfully matched (used to determine 'end' of word)
    checkPath :: CharPos -> Int -> CharPos -> Maybe WordPos
    checkPath currentPos charIdx lastMatchedPos
        | charIdx == wordLen = Just (WordPos startPos lastMatchedPos) -- Word found!
        | not (isValidPos grid currentPos) = Nothing -- Out of bounds
        | otherwise =
            case getCharFromGrid grid currentPos of
                Just charInGrid ->
                    if charInGrid == word !! charIdx
                        then
                            let nextPos = CharPos (col currentPos + dc) (row currentPos + dr)
                            in checkPath nextPos (charIdx + 1) currentPos -- Update lastMatchedPos
                        else Nothing -- Mismatch
                Nothing -> Nothing -- Should not happen if isValidPos is true, but for safety

-- Find a word anywhere in the grid
findWordInGrid :: [String] -> String -> Maybe WordPos
findWordInGrid grid word =
    let numRows = length grid
        numCols = if numRows > 0 then length (head grid) else 0
        -- Generate all possible starting positions in the grid
        allPositions = [CharPos c r | r <- [0..numRows-1], c <- [0..numCols-1]]
    in
    -- Iterate through all possible starting positions
    -- For each position, iterate through all 8 directions
    -- Return the first WordPos found
    foldr (\pos acc -> acc `orElse` (findWordFromPosAndDir' pos)) Nothing allPositions
  where
    -- Helper to try all directions from a given start position
    findWordFromPosAndDir' :: CharPos -> Maybe WordPos
    findWordFromPosAndDir' startPos =
        foldr (\dir acc -> acc `orElse` (findWordFromPosAndDir grid word startPos dir)) Nothing directions

    -- Custom orElse operator for Maybe
    Nothing `orElse` x = x
    Just x  `orElse` _ = Just x

-- Main search function
search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList =
    map (\word -> (word, adjustWordPos (findWordInGrid grid word))) wordList
  where
    -- Adjusts a CharPos from 0-indexed to 1-indexed
    adjustCharPos :: CharPos -> CharPos
    adjustCharPos (CharPos c r) = CharPos (c + 1) (r + 1)

    -- Adjusts a WordPos from 0-indexed to 1-indexed
    adjustWordPos :: Maybe WordPos -> Maybe WordPos
    adjustWordPos Nothing = Nothing
    adjustWordPos (Just (WordPos s e)) =
        Just (WordPos (adjustCharPos s) (adjustCharPos e))
