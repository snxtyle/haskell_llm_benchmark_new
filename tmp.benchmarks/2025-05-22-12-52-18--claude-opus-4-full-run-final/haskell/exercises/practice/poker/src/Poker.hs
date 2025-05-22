module Poker (bestHands) where

import Data.List (sort, sortBy, group, groupBy, maximumBy)
import Data.Ord (comparing, Down(..))
import Data.Maybe (mapMaybe)
import Data.Function (on)

-- Card representation
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
          deriving (Eq, Ord, Enum, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
          deriving (Eq, Show)

data Card = Card { rank :: Rank, suit :: Suit }
          deriving (Eq, Show)

-- Hand ranking types
data HandRank = HighCard [Rank]
              | OnePair Rank [Rank]
              | TwoPair Rank Rank Rank
              | ThreeOfAKind Rank [Rank]
              | Straight Rank
              | Flush [Rank]
              | FullHouse Rank Rank
              | FourOfAKind Rank Rank
              | StraightFlush Rank
              deriving (Eq, Ord, Show)

-- Parse a card string like "2H" or "AS"
parseCard :: String -> Maybe Card
parseCard [r, s] = do
    rank' <- parseRank r
    suit' <- parseSuit s
    return $ Card rank' suit'
parseCard ['1', '0', s] = do
    suit' <- parseSuit s
    return $ Card Ten suit'
parseCard _ = Nothing

parseRank :: Char -> Maybe Rank
parseRank '2' = Just Two
parseRank '3' = Just Three
parseRank '4' = Just Four
parseRank '5' = Just Five
parseRank '6' = Just Six
parseRank '7' = Just Seven
parseRank '8' = Just Eight
parseRank '9' = Just Nine
parseRank 'T' = Just Ten
parseRank 'J' = Just Jack
parseRank 'Q' = Just Queen
parseRank 'K' = Just King
parseRank 'A' = Just Ace
parseRank _ = Nothing

parseSuit :: Char -> Maybe Suit
parseSuit 'C' = Just Clubs
parseSuit 'D' = Just Diamonds
parseSuit 'H' = Just Hearts
parseSuit 'S' = Just Spades
parseSuit _ = Nothing

-- Parse a hand string like "2H 3D 5S 9C KD"
parseHand :: String -> Maybe [Card]
parseHand handStr = 
    let cardStrs = words handStr
        cards = mapMaybe parseCard cardStrs
    in if length cards == 5 then Just cards else Nothing

-- Check if cards form a straight
isStraight :: [Rank] -> Maybe Rank
isStraight ranks =
    let sorted = sort ranks
        isConsecutive rs = all (uncurry isSuccessor) (zip rs (tail rs))
        isSuccessor r1 r2 = fromEnum r2 == fromEnum r1 + 1
    in case sorted of
        [Two, Three, Four, Five, Ace] -> Just Five  -- Ace-low straight
        _ -> if isConsecutive sorted 
             then Just (last sorted)
             else Nothing

-- Check if all cards have the same suit
isFlush :: [Card] -> Bool
isFlush cards = length (group (map suit cards)) == 1

-- Group ranks by frequency
groupRanks :: [Rank] -> [(Int, Rank)]
groupRanks ranks = 
    let grouped = group (sort ranks)
        counts = map (\g -> (length g, head g)) grouped
    in sortBy (comparing (Down . fst) <> comparing (Down . snd)) counts

-- Evaluate a hand to determine its rank
evaluateHand :: [Card] -> HandRank
evaluateHand cards =
    let ranks = map rank cards
        sortedRanks = sortBy (comparing Down) ranks
        grouped = groupRanks ranks
        flush = isFlush cards
        straight = isStraight ranks
    in case (straight, flush, grouped) of
        (Just highRank, True, _) -> StraightFlush highRank
        (_, _, [(4, r), (1, k)]) -> FourOfAKind r k
        (_, _, [(3, r1), (2, r2)]) -> FullHouse r1 r2
        (_, True, _) -> Flush sortedRanks
        (Just highRank, _, _) -> Straight highRank
        (_, _, [(3, r), (1, k1), (1, k2)]) -> 
            ThreeOfAKind r (sortBy (comparing Down) [k1, k2])
        (_, _, [(2, r1), (2, r2), (1, k)]) -> 
            TwoPair (max r1 r2) (min r1 r2) k
        (_, _, [(2, r), (1, k1), (1, k2), (1, k3)]) -> 
            OnePair r (sortBy (comparing Down) [k1, k2, k3])
        _ -> HighCard sortedRanks

-- Find the best hands from a list of hand strings
bestHands :: [String] -> Maybe [String]
bestHands handStrs = do
    let parsedHands = map (\h -> (h, parseHand h)) handStrs
    hands <- sequence [parsed | (_, parsed) <- parsedHands]
    let handsWithRanks = zip handStrs (map evaluateHand hands)
        bestRank = maximum (map snd handsWithRanks)
        bestHandStrs = [h | (h, r) <- handsWithRanks, r == bestRank]
    return bestHandStrs
