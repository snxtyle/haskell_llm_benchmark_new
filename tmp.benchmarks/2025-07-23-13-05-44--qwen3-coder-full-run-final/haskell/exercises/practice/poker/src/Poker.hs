module Poker (bestHands) where

import Data.List (groupBy, sortBy)
import Data.Maybe (mapMaybe)

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands = 
  let validHands = mapMaybe (\h -> case parseHand h of
        Just cards -> Just (h, rankHand cards)
        Nothing -> Nothing) hands
  in if null validHands
     then Nothing
     else Just $ map fst $ filter ((== bestRank) . snd) rankedHands
  where
    rankedHands = map (\(h, cards) -> (h, rankHand cards)) validHands
    bestRank = maximum (map snd rankedHands)

-- Parse a hand string into a list of (value, suit) pairs
parseHand :: String -> Maybe [(Int, Char)]
parseHand hand = 
  let cards = words hand
      parsedCards = map parseCard cards
  in if length cards /= 5 || any (== Nothing) parsedCards
     then Nothing
     else Just (map (maybe undefined id) parsedCards)
  where
    parseCard :: String -> Maybe (Int, Char)
    parseCard [v, s] = 
      case (cardValue [v], s `elem` "CDHS") of
        (Just val, True) -> Just (val, s)
        _ -> Nothing
    parseCard [v1, v2, s] = 
      case (cardValue [v1,v2], s `elem` "CDHS") of
        (Just val, True) -> Just (val, s)
        _ -> Nothing
    parseCard _ = Nothing
    
    cardValue :: String -> Maybe Int
    cardValue "A" = Just 14
    cardValue "K" = Just 13
    cardValue "Q" = Just 12
    cardValue "J" = Just 11
    cardValue v = 
      case reads v of
        [(n, "")] -> if n >= 2 && n <= 10 then Just n else Nothing
        _ -> Nothing

-- Rank a hand according to poker rules
rankHand :: [(Int, Char)] -> (Int, [Int])
rankHand cards = (handRank, tieBreakers)
  where
    values = sortDesc $ map fst cards
    suits = map snd cards
    isFlush = all (== head suits) suits
    isStraight = isStraight' values || isWheel values
    straightValues = if isWheel values then [5,4,3,2,1] else values
    
    (handRank, tieBreakers) 
      | isFlush && isStraight = (8, straightValues)           -- Straight flush
      | isFourOfAKind values  = (7, fourOfAKindTieBreaker values)  -- Four of a kind
      | isFullHouse values    = (6, fullHouseTieBreaker values)    -- Full house
      | isFlush               = (5, values)                   -- Flush
      | isStraight            = (4, straightValues)           -- Straight
      | isThreeOfAKind values = (3, threeOfAKindTieBreaker values) -- Three of a kind
      | isTwoPair values      = (2, twoPairTieBreaker values)      -- Two pair
      | isOnePair values      = (1, onePairTieBreaker values)      -- One pair
      | otherwise             = (0, values)                   -- High card

-- Helper functions
sortDesc :: [Int] -> [Int]
sortDesc = sortBy (flip compare)

isStraight' :: [Int] -> Bool
isStraight' [] = False
isStraight' [_] = False
isStraight' vals = all (\(a,b) -> a - b == 1) $ zip vals (drop 1 vals)

isWheel :: [Int] -> Bool
isWheel [14,5,4,3,2] = True
isWheel _ = False

-- Check for various hand types
isFourOfAKind :: [Int] -> Bool
isFourOfAKind vals = hasNOfAKind 4 vals

isFullHouse :: [Int] -> Bool
isFullHouse vals = hasNOfAKind 3 vals && hasNOfAKind 2 vals

isThreeOfAKind :: [Int] -> Bool
isThreeOfAKind vals = hasNOfAKind 3 vals && not (hasNOfAKind 2 vals)

isTwoPair :: [Int] -> Bool
isTwoPair vals = length (filter (==2) (groupCounts vals)) == 2

isOnePair :: [Int] -> Bool
isOnePair vals = hasNOfAKind 2 vals && not (hasNOfAKind 3 vals)

hasNOfAKind :: Int -> [Int] -> Bool
hasNOfAKind n vals = n `elem` groupCounts vals

groupCounts :: [Int] -> [Int]
groupCounts = map length . groupBy (==) . sortDesc

-- Safe helper functions to avoid partial functions
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

-- Tie breakers for different hand types
fourOfAKindTieBreaker :: [Int] -> [Int]
fourOfAKindTieBreaker vals = 
  let groups = groupBy (==) vals
      quadGroup = filter ((==4) . length) groups
      quad = if null quadGroup then [] else head quadGroup
      rest = filter (`notElem` quad) vals
  in if null quad then vals else quad ++ rest

fullHouseTieBreaker :: [Int] -> [Int]
fullHouseTieBreaker vals = 
  let groups = groupBy (==) vals
      tripsGroup = filter ((==3) . length) groups
      pairGroup = filter ((==2) . length) groups
      trips = if null tripsGroup then [] else head tripsGroup
      pair = if null pairGroup then [] else head pairGroup
  in if null trips || null pair 
     then vals 
     else head trips : [head pair]

threeOfAKindTieBreaker :: [Int] -> [Int]
threeOfAKindTieBreaker vals = 
  let groups = groupBy (==) vals
      tripsGroup = filter ((==3) . length) groups
      trips = if null tripsGroup then [] else head tripsGroup
      rest = filter (`notElem` trips) vals
  in if null trips then vals else head trips : rest

twoPairTieBreaker :: [Int] -> [Int]
twoPairTieBreaker vals = 
  let groups = filter ((==2) . length) $ groupBy (==) vals
      pairs = take 2 groups
      rest = filter (`notElem` concat pairs) vals
  in if length pairs < 2 
     then vals 
     else head (head pairs) : head (last pairs) : rest

onePairTieBreaker :: [Int] -> [Int]
onePairTieBreaker vals = 
  let groups = groupBy (==) vals
      pairGroup = filter ((==2) . length) groups
      pair = if null pairGroup then [] else head pairGroup
      rest = filter (`notElem` pair) vals
  in if null pair then vals else head pair : rest
