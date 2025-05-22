module Alphametics (solve) where

import Data.List (nub, permutations)
import Data.Char (isAlpha)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = 
    case parseEquation puzzle of
        Nothing -> Nothing
        Just (leftTerms, rightTerm) ->
            let letters = nub $ filter isAlpha puzzle
                numLetters = length letters
            in if numLetters > 10 
               then Nothing
               else findSolution letters leftTerms rightTerm

parseEquation :: String -> Maybe ([String], String)
parseEquation puzzle =
    let -- Split on '=' first
        parts = splitOn '=' puzzle
    in case parts of
        [left, right] -> 
            let leftTerms = words $ map (\c -> if c == '+' then ' ' else c) $ filter (\c -> isAlpha c || c == '+') left
                rightTerm = filter isAlpha right
            in if null leftTerms || null rightTerm
               then Nothing
               else Just (leftTerms, rightTerm)
        _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter (c:cs)
    | c == delimiter = "" : rest
    | otherwise = (c : head rest) : tail rest
  where rest = splitOn delimiter cs

findSolution :: [Char] -> [String] -> String -> Maybe [(Char, Int)]
findSolution letters leftTerms rightTerm =
    let numLetters = length letters
        digitPerms = take (factorial numLetters) $ permutations [0..9]
        validPerms = map (take numLetters) digitPerms
    in tryAssignments letters validPerms leftTerms rightTerm

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

tryAssignments :: [Char] -> [[Int]] -> [String] -> String -> Maybe [(Char, Int)]
tryAssignments _ [] _ _ = Nothing
tryAssignments letters (assignment:rest) leftTerms rightTerm =
    let mapping = zip letters assignment
    in if isValidAssignment mapping leftTerms rightTerm
       then Just mapping
       else tryAssignments letters rest leftTerms rightTerm

isValidAssignment :: [(Char, Int)] -> [String] -> String -> Bool
isValidAssignment mapping leftTerms rightTerm =
    let leftNumbers = mapM (wordToNumber mapping) leftTerms
        rightNumber = wordToNumber mapping rightTerm
    in case (leftNumbers, rightNumber) of
        (Just lefts, Just right) -> sum lefts == right
        _ -> False

wordToNumber :: [(Char, Int)] -> String -> Maybe Int
wordToNumber mapping word =
    case word of
        [] -> Nothing
        [c] -> lookup c mapping
        (firstChar:_) -> 
            case lookup firstChar mapping of
                Just 0 -> Nothing  -- No leading zeros for multi-digit numbers
                Just _ -> foldM (\acc char -> 
                    case lookup char mapping of
                        Just d -> Just (acc * 10 + d)
                        Nothing -> Nothing
                    ) 0 word
                Nothing -> Nothing

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM _ acc [] = return acc
foldM f acc (x:xs) = do
    acc' <- f acc x
    foldM f acc' xs
