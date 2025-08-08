module Alphametics (solve) where

import Data.Bits (setBit, testBit)
import Data.List (elemIndex, group, sort, sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

-- Public API
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
  case parsePuzzle puzzle of
    Nothing -> Nothing
    Just (leftWords, rightWord) ->
      let leading = leadingLetters (rightWord:leftWords)
          maxLen  = maximum (map length (rightWord:leftWords))
          -- Start recursive, column-wise search from least-significant column (idx = 0)
          result  = searchColumn leftWords rightWord leading maxLen 0 0 [] 0
      in fmap (sortBy (comparing fst)) result

-- Parse "WORD + WORD == WORD" (or "=") into ([leftWords], rightWord)
parsePuzzle :: String -> Maybe ([String], String)
parsePuzzle s =
  let toks = words s
      eqIdx = case elemIndex "==" toks of
                Just i  -> Just i
                Nothing -> elemIndex "=" toks
   in case eqIdx of
        Nothing -> Nothing
        Just i  ->
          let lhsToks = take i toks
              rhsToks = drop (i + 1) toks
              lhs     = filter (/= "+") lhsToks
              rhsWs   = filter (/= "+") rhsToks
          in case rhsWs of
               (w:_) -> Just (lhs, w)
               _     -> Nothing

-- Letters that cannot be zero (leading letters of multi-digit words)
leadingLetters :: [String] -> [Char]
leadingLetters ws = uniq [head w | w <- ws, length w > 1]

-- Uniqueness helper (keeps first occurrence order)
uniq :: (Eq a) => [a] -> [a]
uniq = go []
  where
    go _    []     = []
    go seen (x:xs) = if x `elem` seen then go seen xs else x : go (x:seen) xs

-- Column-wise recursive search
-- idx: current column from right (0-based)
-- carry: carry coming into this column
-- assigned: association list [(Char, Int)]
-- used: bitset of used digits (bit d set means digit d is already used)
searchColumn
  :: [String]              -- left words
  -> String                -- right word
  -> [Char]                -- leading letters (cannot be 0 if multi-digit)
  -> Int                   -- max length over all words
  -> Int                   -- current column index (0 = least-significant)
  -> Int                   -- incoming carry
  -> [(Char, Int)]         -- assigned mapping
  -> Int                   -- used digits bitset
  -> Maybe [(Char, Int)]   -- solution mapping
searchColumn leftWords rightWord leading maxLen idx carry assigned used
  | idx == maxLen =
      if carry == 0
        then Just assigned
        else Nothing
  | otherwise =
      let leftLetters = lettersAtColumn leftWords idx
          rightLetter = letterAtColumn rightWord idx  -- Maybe Char

          -- Count occurrences of each left letter in this column
          counts = countChars leftLetters  -- [(Char, Int)]
          -- Split into (knownSum, unknownLeftWithCounts)
          (knownSum, unknownLeft) =
            foldl
              (\(accSum, unk) (c, cnt) ->
                  case lookup c assigned of
                    Just d  -> (accSum + cnt * d, unk)
                    Nothing -> (accSum, (c, cnt) : unk))
              (0, [])
              counts

          -- Try all assignments of unknown left letters at this column
          unknownLeftSorted = sortBy (flip (comparing snd)) unknownLeft
          leftAssignments = assignUnknownLeft unknownLeftSorted assigned used leading 0
      in firstJust
            [ let v = knownSum + ulSum + carry
                  rDigit = v `mod` 10
                  nextCarry = v `div` 10
               in case rightLetter of
                    Nothing ->
                      if rDigit == 0
                        then searchColumn leftWords rightWord leading maxLen (idx + 1) nextCarry asgn u
                        else Nothing
                    Just r ->
                      case lookup r asgn of
                        Just dR ->
                          if dR == rDigit
                            then searchColumn leftWords rightWord leading maxLen (idx + 1) nextCarry asgn u
                            else Nothing
                        Nothing ->
                          if testBit u rDigit then Nothing
                          else if rDigit == 0 && r `elem` leading then Nothing
                          else
                            let asgn' = (r, rDigit) : asgn
                                u'    = setBit u rDigit
                            in searchColumn leftWords rightWord leading maxLen (idx + 1) nextCarry asgn' u'
            | (asgn, u, ulSum) <- leftAssignments
            ]

-- Assign digits to unknown left letters for one column.
-- Returns a list of (newAssigned, newUsed, contributionSum)
assignUnknownLeft
  :: [(Char, Int)]         -- unknown letters with multiplicity at this column
  -> [(Char, Int)]         -- assigned mapping so far
  -> Int                   -- used digits bitset
  -> [Char]                -- leading letters (cannot be 0)
  -> Int                   -- accumulated contribution sum for already assigned in this column
  -> [ ([(Char, Int)], Int, Int) ]
assignUnknownLeft [] asgn used _ accSum = [(asgn, used, accSum)]
assignUnknownLeft ((c, cnt) : rest) asgn used leading accSum =
  [ res
  | d <- [0..9]
  , not (testBit used d)
  , not (d == 0 && c `elem` leading)
  , let asgn' = (c, d) : asgn
        used' = setBit used d
        acc'  = accSum + cnt * d
  , res <- assignUnknownLeft rest asgn' used' leading acc'
  ]

-- Letters present at a given column (from right) for all left words
lettersAtColumn :: [String] -> Int -> [Char]
lettersAtColumn ws idx = [ ch | w <- ws, Just ch <- [letterAtColumn w idx] ]

-- Letter at given column from right for a single word
letterAtColumn :: String -> Int -> Maybe Char
letterAtColumn w idx =
  let n = length w
  in if idx < n then Just (w !! (n - 1 - idx)) else Nothing

-- Count occurrences of characters
countChars :: [Char] -> [(Char, Int)]
countChars cs = [ (head g, length g) | g <- group (sort cs) ]

-- Return the first Just value from a list, if any
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) = case x of
  Just v  -> Just v
  Nothing -> firstJust xs
