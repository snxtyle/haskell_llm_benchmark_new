module Alphametics (solve) where

import Data.List (nub, delete, find)
import Data.Char (isUpper)
import Data.Maybe (listToMaybe)

solve :: String -> Maybe [(Char, Int)]
solve puzzle = listToMaybe $ solveAll puzzle

solveAll :: String -> [[(Char, Int)]]
solveAll puzzle = 
    case parsePuzzle puzzle of
        Nothing -> []
        Just (addends, result) -> 
            let variables = nub $ filter isUpper $ concat (result : addends)
                leadingChars = nub $ map head $ filter (not . null) (result : addends)
            in map (zip variables) $ findSolutions addends result variables leadingChars

parsePuzzle :: String -> Maybe ([String], String)
parsePuzzle puzzle = 
    case break (== '=') (reverse $ filter (not . flip elem " \t") puzzle) of
        (_, '=':' ':rest) -> 
            let leftSide = reverse rest
                (rightSide, _) = break (== '=') (reverse puzzle)
                rightSideClean = reverse $ drop 1 rightSide
                addends = splitByPlus leftSide
            in Just (addends, rightSideClean)
        _ -> Nothing
  where
    splitByPlus [] = []
    splitByPlus s = 
        case break (== '+') s of
            (word, []) -> [filter (/= ' ') word]
            (word, _:rest) -> filter (/= ' ') word : splitByPlus rest

findSolutions :: [String] -> String -> [Char] -> [Char] -> [[Int]]
findSolutions addends result variables leadingChars = 
    filter (isValidAssignment addends result leadingChars) $ 
    assignments variables [0..9]

assignments :: [Char] -> [Int] -> [[Int]]
assignments [] _ = [[]]
assignments (v:vs) digits = 
    [d:rest | d <- digits, rest <- assignments vs (delete d digits)]

isValidAssignment :: [String] -> String -> [Char] -> [Int] -> Bool
isValidAssignment addends result leadingChars assignment = 
    let variables = nub $ concat (result : addends)
        varMap = zip variables assignment
        hasLeadingZero = any (\c -> lookup c varMap == Just 0) leadingChars
    in not hasLeadingZero && checkEquation addends result varMap

checkEquation :: [String] -> String -> [(Char, Int)] -> Bool
checkEquation addends result varMap = 
    let addendValues = map (wordToNumber varMap) addends
        resultValue = wordToNumber varMap result
    in all (/= -1) (resultValue : addendValues) && sum addendValues == resultValue

wordToNumber :: [(Char, Int)] -> String -> Int
wordToNumber varMap word = 
    case mapM (flip lookup varMap) word of
        Nothing -> -1
        Just digits -> foldl (\acc d -> acc * 10 + d) 0 digits
