module Poker (bestHands) where

import Data.List (group, sort, sortBy)

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands = Just $ map fst $ filter (\(_, val) -> val == maxValue) handValues
  where
    handValues = map (\hand -> (hand, evaluateHand hand)) hands
    maxValue = maximum $ map snd handValues

evaluateHand :: String -> (Int, [Int])
evaluateHand handStr = 
  let cards = parseHand handStr
      ranks = map fst cards
      suits = map snd cards
      flush = allSame suits
      straightMay = getStraightHighCard ranks
      groups = group $ sort ranks
      groups' = map (\g -> (length g, head g)) groups
      sortGroup (len1, r1) (len2, r2) =
        case compare len2 len1 of
          EQ -> compare r2 r1
          cmp -> cmp
      sortedGroups = sortBy sortGroup groups'
      tieBreaker = map snd sortedGroups
  in
    if flush && straightMay /= Nothing 
      then (9, [fromJust straightMay])
    else if straightMay /= Nothing 
      then (5, [fromJust straightMay])
    else if flush
      then (6, tieBreaker)
    else case (map fst sortedGroups) of
        [4,1] -> (8, tieBreaker)
        [3,2] -> (7, tieBreaker)
        [3,1,1] -> (4, tieBreaker)
        [2,2,1] -> (3, tieBreaker)
        [2,1,1,1] -> (2, tieBreaker)
        _ -> (1, tieBreaker)

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "Unexpected Nothing in fromJust"

allSame :: (Eq a) => [a] -> Bool
allSame (x:xs) = all (==x) xs
allSame [] = True

getStraightHighCard :: [Int] -> Maybe Int
getStraightHighCard ranks = 
  let sorted = sortBy (flip compare) ranks
      straightNormal = case makeConsecutive sorted of
        [] -> False
        xs@(first:_) -> xs == [first, first-1..first-4]
      straightAceLow = sorted == [14,5,4,3,2]
  in 
    if straightNormal then Just $ maximum sorted
    else if straightAceLow then Just 5
    else Nothing

makeConsecutive :: [Int] -> [Int]
makeConsecutive (x:xs) = x : foldl (\acc y -> if y == last acc - 1 then acc ++ [y] else acc) [x] xs

parseHand :: String -> [(Int, Char)]
parseHand handStr = 
  map parseCard $ words handStr
  where
    parseCard cardStr = 
      let suit = last cardStr
          rankStr = take (length cardStr - 1) cardStr
      in (parseRank rankStr, suit)

    parseRank :: String -> Int
    parseRank "2" = 2
    parseRank "3" = 3
    parseRank "4" = 4
    parseRank "5" = 5
    parseRank "6" = 6
    parseRank "7" = 7
    parseRank "8" = 8
    parseRank "9" = 9
    parseRank "10" = 10
    parseRank "T" = 10
    parseRank "J" = 11
    parseRank "Q" = 12
    parseRank "K" = 13
    parseRank "A" = 14
    parseRank r = error $ "Invalid rank: " ++ r
