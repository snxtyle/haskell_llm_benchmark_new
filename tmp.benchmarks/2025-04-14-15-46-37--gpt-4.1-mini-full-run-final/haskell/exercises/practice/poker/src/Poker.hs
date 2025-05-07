module Poker (bestHands) where

import Data.List
import Data.Maybe

-- Card ranks in order from low to high
ranks :: String
ranks = "23456789TJQKA"

-- Convert rank character to its numeric value
rankValue :: Char -> Int
rankValue c = fromJust $ elemIndex c ranks

-- Parse a card string like "4S" into (rank, suit)
type Card = (Char, Char)

parseCard :: String -> Card
parseCard [r,s]
  | r `elem` ranks && s `elem` "CDHS" = (r,s)
  | otherwise = error "Invalid card"
parseCard _ = error "Invalid card"

-- Parse a hand string like "4S 5S 7H 8D JC" into list of cards
parseHand :: String -> [Card]
parseHand = map parseCard . words

-- Count occurrences of each rank in a hand
rankCounts :: [Card] -> [Int]
rankCounts hand = sortBy (flip compare) $ map length $ group $ sort $ map fst hand

-- Check if all cards have the same suit
isFlush :: [Card] -> Bool
isFlush hand = all ((== head suits) . id) suits
  where suits = map snd hand

-- Check if ranks form a straight
isStraight :: [Card] -> Bool
isStraight hand = consecutive sortedRanks || lowAceStraight
  where
    sortedRanks = sort $ map rankValue $ map fst hand
    consecutive xs = and $ zipWith (\a b -> b == a + 1) xs (tail xs)
    -- Special case: A-2-3-4-5 straight (Ace low)
    lowAceStraight = sort (map fst hand) == "A2345"

-- Get the highest rank in the hand, considering Ace low straight
highestRank :: [Card] -> Int
highestRank hand
  | lowAceStraight = rankValue '5'
  | otherwise = maximum $ map rankValue $ map fst hand
  where
    lowAceStraight = sort (map fst hand) == "A2345"

-- Determine the hand rank and tiebreaker info
-- Returns a tuple where higher tuple means better hand
handRank :: [Card] -> (Int, [Int])
handRank hand
  | isFlush hand && isStraight hand = (8, [highestRank hand]) -- Straight flush
  | counts == [4,1] = (7, rankGroups) -- Four of a kind
  | counts == [3,2] = (6, rankGroups) -- Full house
  | isFlush hand = (5, rankGroups) -- Flush
  | isStraight hand = (4, [highestRank hand]) -- Straight
  | counts == [3,1,1] = (3, rankGroups) -- Three of a kind
  | counts == [2,2,1] = (2, rankGroups) -- Two pairs
  | counts == [2,1,1,1] = (1, rankGroups) -- One pair
  | otherwise = (0, rankGroups) -- High card
  where
    counts = rankCounts hand
    -- rankGroups: ranks sorted by count desc, then rank desc
    rankGroups = map (rankValue . head) $ sortBy cmp $ group $ sort $ map fst hand
    cmp a b = compare (length b) (length a) <> compare (rankValue (head b)) (rankValue (head a))

-- Return the best hand(s) from a list of hands
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands = Just $ filter ((== bestRank) . handRank . parseHand) hands
  where
    bestRank = maximum $ map (handRank . parseHand) hands
