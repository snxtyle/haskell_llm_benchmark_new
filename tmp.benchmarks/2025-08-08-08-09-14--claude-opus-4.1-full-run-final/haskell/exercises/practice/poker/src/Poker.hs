module Poker (bestHands) where

import Data.List (group, sort, sortBy, maximumBy)
import Data.Ord (comparing, Down(..))
import Data.Maybe (mapMaybe)
import Data.Char (isDigit)

-- Card representation
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
          deriving (Eq, Ord, Enum, Show)

data Suit = Clubs | Diamonds | Hearts | Spades
          deriving (Eq, Show)

data Card = Card Rank Suit
          deriving (Eq, Show)

-- Hand ranking from highest to lowest
data HandRank = HighCard [Rank]
              | OnePair Rank [Rank]
              | TwoPair Rank Rank [Rank]
              | ThreeOfAKind Rank [Rank]
              | Straight Rank
              | Flush [Rank]
              | FullHouse Rank Rank
              | FourOfAKind Rank [Rank]
              | StraightFlush Rank
              deriving (Eq, Ord, Show)

-- Parse a card from string
parseCard :: String -> Maybe Card
parseCard [] = Nothing
parseCard [_] = Nothing
parseCard s = do
    let (rankStr, suitStr) = splitAt (length s - 1) s
    rank <- parseRank rankStr
    suit <- parseSuit suitStr
    return $ Card rank suit

parseRank :: String -> Maybe Rank
parseRank "2" = Just Two
parseRank "3" = Just Three
parseRank "4" = Just Four
parseRank "5" = Just Five
parseRank "6" = Just Six
parseRank "7" = Just Seven
parseRank "8" = Just Eight
parseRank "9" = Just Nine
parseRank "10" = Just Ten
parseRank "J" = Just Jack
parseRank "Q" = Just Queen
parseRank "K" = Just King
parseRank "A" = Just Ace
parseRank _ = Nothing

parseSuit :: String -> Maybe Suit
parseSuit "C" = Just Clubs
parseSuit "D" = Just Diamonds
parseSuit "H" = Just Hearts
parseSuit "S" = Just Spades
parseSuit _ = Nothing

-- Parse a hand from string
parseHand :: String -> Maybe [Card]
parseHand s = sequence $ map parseCard $ words s

-- Get ranks from cards
getRanks :: [Card] -> [Rank]
getRanks = map (\(Card r _) -> r)

-- Get suits from cards
getSuits :: [Card] -> [Suit]
getSuits = map (\(Card _ s) -> s)

-- Check if all suits are the same
isFlush :: [Card] -> Bool
isFlush cards = length (group $ getSuits cards) == 1

-- Check if ranks form a straight
isStraight :: [Rank] -> Bool
isStraight ranks = 
    let sorted = sort ranks
        isConsecutive rs = and $ zipWith (\a b -> fromEnum b == fromEnum a + 1) rs (tail rs)
    in isConsecutive sorted || sorted == [Two, Three, Four, Five, Ace] -- Ace-low straight

-- Get the high card of a straight (handles Ace-low straight)
getStraightHigh :: [Rank] -> Rank
getStraightHigh ranks =
    let sorted = sort ranks
    in if sorted == [Two, Three, Four, Five, Ace]
       then Five  -- In Ace-low straight, Five is the high card
       else maximum ranks

-- Group ranks by frequency
groupRanks :: [Rank] -> [(Int, Rank)]
groupRanks ranks = 
    let grouped = group $ sort ranks
    in sortBy (comparing (Down . fst)) $ map (\g -> (length g, head g)) grouped

-- Evaluate a hand to get its rank
evaluateHand :: [Card] -> HandRank
evaluateHand cards =
    let ranks = getRanks cards
        sortedRanks = sortBy (comparing Down) ranks
        grouped = groupRanks ranks
        isFlushHand = isFlush cards
        isStraightHand = isStraight ranks
    in case (isFlushHand, isStraightHand, grouped) of
        (True, True, _) -> StraightFlush (getStraightHigh ranks)
        (_, _, (4, r):rest) -> FourOfAKind r (map snd rest)
        (_, _, (3, r1):(2, r2):_) -> FullHouse r1 r2
        (True, _, _) -> Flush sortedRanks
        (_, True, _) -> Straight (getStraightHigh ranks)
        (_, _, (3, r):rest) -> ThreeOfAKind r (map snd rest)
        (_, _, (2, r1):(2, r2):rest) -> TwoPair (max r1 r2) (min r1 r2) (map snd rest)
        (_, _, (2, r):rest) -> OnePair r (map snd rest)
        _ -> HighCard sortedRanks

-- Find the best hands from a list of hand strings
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands = 
    let parsedHands = mapMaybe parseHand hands
        handsWithRanks = zip hands $ map evaluateHand parsedHands
    in if null handsWithRanks
       then Nothing
       else let maxRank = maximum $ map snd handsWithRanks
                bestHandsList = [h | (h, r) <- handsWithRanks, r == maxRank]
            in Just bestHandsList
