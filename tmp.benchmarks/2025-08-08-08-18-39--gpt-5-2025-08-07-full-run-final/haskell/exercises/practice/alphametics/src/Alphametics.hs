module Alphametics (solve) where

import Data.Char (isSpace)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Assignment = M.Map Char Int
type UsedDigits = S.Set Int

solve :: String -> Maybe [(Char, Int)]
solve puzzle = do
  (addends, result) <- parseEquation puzzle
  let allLetters = S.fromList (concat (result : addends))
  if S.size allLetters > 10
    then Nothing
    else
      let leadingLetters = S.fromList [head w | w <- result : addends, length w > 1]
          columns = buildColumns addends result
      in fmap (sortOn fst . M.toList) (dfs columns 0 0 M.empty S.empty leadingLetters)

-- Depth-first search over columns from least significant to most significant.
dfs :: [([Char], Maybe Char)] -> Int -> Int -> Assignment -> UsedDigits -> S.Set Char -> Maybe Assignment
dfs columns idx carry asg used leading
  | idx == length columns = if carry == 0 then Just asg else Nothing
  | otherwise =
      let (adders, mRes) = columns !! idx
          (sumKnown, unknownCounts) = analyzeColumn adders asg
      in tryAssignUnknowns (M.toList unknownCounts) asg used 0
         where
           -- Assign digits to unknown adder letters (with multiplicity) in this column.
           tryAssignUnknowns :: [(Char, Int)] -> Assignment -> UsedDigits -> Int -> Maybe Assignment
           tryAssignUnknowns [] asg' used' sumUnknown = handleResult asg' used' sumUnknown
           tryAssignUnknowns ((ch, mult):rest) asg' used' sumUnknown =
             firstJust
               [ let asg''  = M.insert ch d asg'
                     used'' = S.insert d used'
                 in tryAssignUnknowns rest asg'' used'' (sumUnknown + mult * d)
               | d <- candidateDigits ch used' leading
               ]

           handleResult :: Assignment -> UsedDigits -> Int -> Maybe Assignment
           handleResult asg' used' sumUnknown =
             let total     = sumKnown + sumUnknown + carry
                 resDigit  = total `mod` 10
                 nextCarry = total `div` 10
             in case mRes of
                  Nothing ->
                    if resDigit == 0
                      then dfs columns (idx + 1) nextCarry asg' used' leading
                      else Nothing
                  Just r ->
                    case M.lookup r asg' of
                      Just d ->
                        if d == resDigit
                          then dfs columns (idx + 1) nextCarry asg' used' leading
                          else Nothing
                      Nothing ->
                        if S.member resDigit used'
                           || (resDigit == 0 && S.member r leading)
                          then Nothing
                          else
                            let asg''  = M.insert r resDigit asg'
                                used'' = S.insert resDigit used'
                            in dfs columns (idx + 1) nextCarry asg'' used'' leading

-- Analyze a column: sum of known digits and multiplicities of unknown letters.
analyzeColumn :: [Char] -> Assignment -> (Int, M.Map Char Int)
analyzeColumn letters asg = foldl step (0, M.empty) letters
  where
    step (sumKnown, counts) ch =
      case M.lookup ch asg of
        Just d  -> (sumKnown + d, counts)
        Nothing -> (sumKnown, M.insertWith (+) ch 1 counts)

-- Digits available for a given letter, respecting uniqueness and leading-zero rule.
candidateDigits :: Char -> UsedDigits -> S.Set Char -> [Int]
candidateDigits ch used leading =
  [ d
  | d <- [0 .. 9]
  , not (S.member d used)
  , not (d == 0 && S.member ch leading)
  ]

-- Build columns from least-significant (index 0) to most-significant.
-- Each column is the list of adder letters at that position and an optional result letter.
buildColumns :: [String] -> String -> [([Char], Maybe Char)]
buildColumns addends result =
  [ ( [ w !! (length w - 1 - k) | w <- addends, length w > k ]
    , if length result > k then Just (result !! (length result - 1 - k)) else Nothing
    )
  | k <- [0 .. maxLen - 1]
  ]
  where
    maxLen = maximum (map length (result : addends))

-- Parse equation of the form "WORD + WORD + ... == WORD" (also accepts single '=').
parseEquation :: String -> Maybe ([String], String)
parseEquation s =
  case break (== '=') compact of
    (lhs, '=':'=':rhs) -> mkParts lhs rhs
    (lhs, '=':rhs)     -> mkParts lhs rhs
    _                  -> Nothing
  where
    compact = filter (not . isSpace) s
    mkParts lhs rhs =
      let addends = splitOn '+' lhs
      in if null addends || null rhs
           then Nothing
           else Just (addends, rhs)

-- Simple split on a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delim (c:cs)
  | c == delim = "" : rest
  | otherwise  = (c : head rest) : tail rest
  where
    rest = splitOn delim cs

-- Return the first Just value from a list or Nothing.
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (x:xs) = case x of
  Just _  -> x
  Nothing -> firstJust xs
