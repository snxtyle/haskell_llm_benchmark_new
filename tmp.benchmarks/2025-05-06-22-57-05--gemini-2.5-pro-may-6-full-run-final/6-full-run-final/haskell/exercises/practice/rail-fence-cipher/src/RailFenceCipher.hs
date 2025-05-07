module RailFenceCipher (encode, decode) where

import Data.List (sortOn)

-- Generates the sequence of rail indices for a message of a given length.
-- Example: numRails = 3, msgLen = 7 -> [0,1,2,1,0,1,2]
-- Pattern for 3 rails: 0,1,2,1 (cycle length 4)
generateRailIndices :: Int -> Int -> [Int]
generateRailIndices numRails msgLen
  -- numRails is assumed to be >= 1 by callers (encode/decode),
  -- or handled by their specific numRails <= 0 / numRails == 1 checks.
  -- msgLen == 0 is handled by `take msgLen $ cycle ...` correctly returning [].
  | numRails == 1 = replicate msgLen 0
  | otherwise = -- numRails >= 2
      -- upAndPeak: e.g., numRails=3 -> [0,1,2]
      let upAndPeak = [0 .. numRails - 1]
          -- downSlope: e.g., numRails=3 -> [1] (reverse $ init $ tail [0,1,2] == reverse $ init [1,2] == reverse [1] == [1])
          --            e.g., numRails=2 -> []  (reverse $ init $ tail [0,1] == reverse $ init [1] == reverse [] == [])
          downSlope = reverse $ init $ tail upAndPeak
          -- pattern: e.g., numRails=3 -> [0,1,2,1]
          --          e.g., numRails=2 -> [0,1]
          pattern = upAndPeak ++ downSlope
      in take msgLen $ cycle pattern

encode :: Int -> String -> String
encode numRails msg
  | null msg = ""
  | numRails <= 0 = msg -- As per problem, rails are positive. Could also be: error "Number of rails must be positive"
  | numRails == 1 = msg
  -- If numRails is high, each char is on its own rail effectively.
  | numRails >= length msg = msg 
  | otherwise =
    let railIndices = generateRailIndices numRails (length msg) -- Indices for each char
        charsWithRails = zip msg railIndices -- Associate char with its rail index, e.g. [('W',0), ('E',1), ...]
        
        -- For each rail, collect characters belonging to it, in order.
        -- e.g., for rail 0: [char | (char, rIdx) <- charsWithRails, rIdx == 0] -> "WECRLTE"
        charsForRail :: Int -> String
        charsForRail railIdxToCollect = [char | (char, rIdx) <- charsWithRails, rIdx == railIdxToCollect]
        
        -- Concatenate characters from rail 0, then rail 1, ...
        -- e.g., map charsForRail [0,1,2] -> ["WECRLTE", "ERDSOEEFEAOC", "AIVDEN"]
        allRailsContent :: [String]
        allRailsContent = map charsForRail [0 .. numRails - 1]
    in concat allRailsContent -- Flattens [[Char]] into [Char] (String)

decode :: Int -> String -> String
decode numRails ciphertext
  | null ciphertext = ""
  | numRails <= 0 = ciphertext -- As per problem, rails are positive. Could also be: error "Number of rails must be positive"
  | numRails == 1 = ciphertext
  -- If numRails is high, each char is on its own rail effectively.
  | numRails >= length ciphertext = ciphertext
  | otherwise =
    let msgLen = length ciphertext
        
        -- 1. Determine the rail index for each character in the *original* message sequence.
        --    e.g., for 3 rails, msgLen=26 (from example): [0,1,2,1,0,1,2,1,0,1,2,1,0,1,2,1,0,1,2,1,0,1,2,1,0,1]
        railVisitOrder = generateRailIndices numRails msgLen

        -- 2. Calculate how many characters belong to each rail.
        --    e.g., for railVisitOrder above, numRails=3:
        --    Rail 0 count: 7
        --    Rail 1 count: 12
        --    Rail 2 count: 7
        --    railCounts = [7,12,7]
        countOccurrences :: Eq a => a -> [a] -> Int
        countOccurrences target list = length $ filter (== target) list
        railCounts = map (\rIdx -> countOccurrences rIdx railVisitOrder) [0 .. numRails - 1]

        -- 3. Split the ciphertext into segments according to railCounts.
        --    e.g., ciphertext="WECRLTEERDSOEEFEAOCAIVDEN", railCounts=[7,12,7]
        --    cipherSegments = ["WECRLTE", "ERDSOEEFEAOC", "AIVDEN"]
        splitIntoSegments :: [a] -> [Int] -> [[a]]
        splitIntoSegments _ [] = []
        splitIntoSegments currentCiphertext (lenCurrentSegment:remainingLens) =
            let (segment, restOfCiphertext) = splitAt lenCurrentSegment currentCiphertext
            in segment : splitIntoSegments restOfCiphertext remainingLens
        
        cipherSegments = splitIntoSegments ciphertext railCounts -- This is [[Char]], i.e., [String]

        -- 4. Pair original indices (0 to msgLen-1) with their rail numbers from railVisitOrder.
        --    e.g., msgLen=26, railVisitOrder=[0,1,2,1,...]
        --    indexedRails = [(0,0), (1,1), (2,2), (3,1), (4,0), ...]
        indexedRails = zip [0..msgLen-1] railVisitOrder

        -- 5. For each rail, get the list of original indices that belong to it.
        --    e.g., Rail 0: [0,4,8,12,16,20,24]
        --          Rail 1: [1,3,5,7,9,11,13,15,17,19,21,23,25]
        --          Rail 2: [2,6,10,14,18,22]
        originalIndicesByRail :: [[Int]]
        originalIndicesByRail = map (\rIdxToCollect -> [origIdx | (origIdx, railNum) <- indexedRails, railNum == rIdxToCollect]) [0 .. numRails - 1]

        -- 6. Zip these lists of original indices with the characters from cipherSegments.
        --    This creates a list of (original_index, char) pairs for each rail.
        --    e.g., Rail 0: zip [0,4,...] "WECRLTE" -> [(0,'W'),(4,'E'),(8,'C'),...]
        --          Rail 1: zip [1,3,...] "ERDSOEEFEAOC" -> [(1,'E'),(3,'R'),(5,'D'),...]
        positionedCharsGroupedByRail :: [[(Int, Char)]]
        positionedCharsGroupedByRail = zipWith zip originalIndicesByRail cipherSegments

        -- 7. Concatenate all these pairs into a single list.
        --    e.g., [(0,'W'),(4,'E'),...,(1,'E'),(3,'R'),...]
        allPositionedChars :: [(Int, Char)]
        allPositionedChars = concat positionedCharsGroupedByRail

        -- 8. Sort these pairs by their original index.
        --    e.g., [(0,'W'),(1,'E'),(2,'A'),(3,'R'),(4,'E'),...]
        sortedPositionedChars = sortOn fst allPositionedChars

        -- 9. Extract the characters in the sorted order to get the decoded message.
        --    e.g., "WEAREDISCOVEREDFLEEATONCE"
    in map snd sortedPositionedChars
