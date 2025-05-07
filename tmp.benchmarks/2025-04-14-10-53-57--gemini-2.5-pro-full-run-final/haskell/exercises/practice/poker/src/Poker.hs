module Poker (bestHands) where

import Data.List (sort, sortBy, group)
import Data.Maybe (mapMaybe, fromJust, sequence) -- Added sequence
import Data.Ord (comparing, Down(..))
-- Removed unused Control.Applicative import

-- Data Types

data Suit = Spade | Heart | Diamond | Club
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Show)

-- Make Card Ord based on Rank only for sorting within a hand
instance Ord Card where
  compare = comparing rank

type Hand = [Card] -- Expecting 5 cards

-- Hand Ranking Type (Ordered from Lowest to Highest for derived Ord)
data HandRank = HighCard [Rank] -- List of ranks, highest first
              | OnePair Rank [Rank] -- Pair rank, kicker ranks (highest first)
              | TwoPair Rank Rank Rank -- Higher pair rank, lower pair rank, kicker rank
              | ThreeOfAKind Rank [Rank] -- Rank of the three, kicker ranks (highest first)
              | Straight Rank -- Highest card rank
              | Flush [Rank] -- List of ranks, highest first
              | FullHouse Rank Rank -- Rank of the three, rank of the pair
              | FourOfAKind Rank Rank -- Rank of the four, kicker rank
              | StraightFlush Rank -- Highest card rank
              deriving (Eq, Ord, Show) -- Ord is derived based on constructor order

-- Parsing

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
parseRank _   = Nothing

parseSuit :: Char -> Maybe Suit
parseSuit 'S' = Just Spade
parseSuit 'H' = Just Heart
parseSuit 'D' = Just Diamond
parseSuit 'C' = Just Club
parseSuit _   = Nothing

parseCard :: String -> Maybe Card
parseCard [r, s] = Card <$> parseRank r <*> parseSuit s
parseCard [r1, r2, s] | [r1, r2] == "10" = Card <$> parseRank 'T' <*> parseSuit s -- Handle "10"
parseCard _      = Nothing

parseHand :: String -> Maybe Hand
parseHand str =
  let cardStrings = words str
      maybeCards = map parseCard cardStrings
  in if length cardStrings /= 5 -- Check for exactly 5 card strings
     then Nothing
     -- Use sequence to convert [Maybe Card] to Maybe [Card]
     -- This also implicitly checks if any parseCard returned Nothing
     else sequence maybeCards

-- Hand Evaluation Helpers

sortedRanks :: Hand -> [Rank]
sortedRanks = sortBy (comparing Down) . map rank

rankCounts :: Hand -> [(Rank, Int)]
rankCounts hand = sortBy (comparing (Down . snd) <> comparing (Down . fst)) -- Sort by count desc, then rank desc
                $ map (\g -> (rank $ head g, length g))
                $ group $ sort hand -- Group by rank

isFlush :: Hand -> Bool
isFlush hand = case hand of
  (c:cs) -> all ((== suit c) . suit) cs
  []     -> True -- Technically true for an empty hand, but we expect 5 cards

-- Check for straight, returns highest rank if straight, handles Ace-low
isStraight :: [Rank] -> Maybe Rank
isStraight ranks =
  let uniqueSortedRanks = reverse $ sort $ ranks -- High to low
      isConsecutive = all (\(x, y) -> fromEnum x == fromEnum y + 1) $ zip uniqueSortedRanks (tail uniqueSortedRanks)
      isAceLow = uniqueSortedRanks == [Ace, Five, Four, Three, Two]
  in if isConsecutive then Just (head uniqueSortedRanks) -- Highest card in straight
     else if isAceLow then Just Five -- Ace-low straight (A, 2, 3, 4, 5), rank is Five
     else Nothing

-- Hand Evaluation Function

evaluateHand :: Hand -> HandRank
evaluateHand hand =
  let ranks = sortedRanks hand -- Ranks sorted high to low
      counts = rankCounts hand
      flush = isFlush hand
      maybeStraightRank = isStraight ranks
      -- Ace-low straight check specifically for straight flush
      isAceLowStraight = ranks == [Ace, Five, Four, Three, Two]
      maybeStraightFlushRank = if flush && isAceLowStraight then Just Five else if flush then maybeStraightRank else Nothing

  in case (maybeStraightFlushRank, counts, flush, maybeStraightRank) of
    (Just r, _, _, _)       -> StraightFlush r -- Straight Flush (includes Ace-low)
    (_, (r4, 4):_, _, _)    -> let kicker = head [r | (r, 1) <- counts] in FourOfAKind r4 kicker -- Four of a Kind
    (_, (r3, 3):(r2, 2):_, _, _) -> FullHouse r3 r2 -- Full House
    (_, _, True, _)         -> Flush ranks -- Flush
    (_, _, _, Just r)       -> Straight r -- Straight (already handled Ace-low in isStraight)
    (_, (r3, 3):_, _, _)    -> let kickers = [r | (r, 1) <- counts] in ThreeOfAKind r3 kickers -- Three of a Kind
    (_, (r2a, 2):(r2b, 2):_, _, _) -> let kicker = head [r | (r, 1) <- counts] in TwoPair r2a r2b kicker -- Two Pair
    (_, (r2, 2):_, _, _)    -> let kickers = [r | (r, 1) <- counts] in OnePair r2 kickers -- One Pair
    _                       -> HighCard ranks -- High Card
  -- Removed local fromJust definition

-- Main Function

bestHands :: [String] -> Maybe [String]
bestHands handStrings = do -- Using Maybe monad
  -- Use sequence to handle potential parsing errors for any hand
  parsedHandData <- sequence $ map (\s -> (,) <$> parseHand s <*> pure s) handStrings -- Maybe [(Hand, String)]
  case parsedHandData of
    Nothing -> Nothing -- If any hand failed to parse, return Nothing
    Just [] -> Just [] -- Handle empty input list case explicitly
    Just validHands -> -- validHands is [(Hand, String)]
      if null validHands then Just [] -- Check if the list is empty after potential filtering (though sequence handles this)
      else
        let evaluatedHands = map (\(hand, str) -> (evaluateHand hand, str)) validHands -- [(HandRank, String)]
            maxRank = maximum $ map fst evaluatedHands
            -- Renamed 'rank' to 'handRank' to avoid shadowing
            bestHandStrings = [str | (handRank, str) <- evaluatedHands, handRank == maxRank]
        in Just bestHandStrings
