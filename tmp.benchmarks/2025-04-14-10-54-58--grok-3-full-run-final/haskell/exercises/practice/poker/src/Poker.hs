{-# LANGUAGE TupleSections #-}

module Poker (bestHands) where

import Data.List (group, sort, sortBy)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

-- Card data types
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
          deriving (Eq, Ord, Enum, Show)
data Card = Card Rank Suit deriving (Eq, Show)

-- Hand ranking categories
data HandRank = HighCard [Rank]
              | Pair Rank [Rank]
              | TwoPair Rank Rank [Rank]
              | ThreeOfAKind Rank [Rank]
              | Straight Rank
              | Flush [Rank]
              | FullHouse Rank Rank
              | FourOfAKind Rank Rank
              | StraightFlush Rank
              deriving (Eq, Show)

instance Ord HandRank where
  compare (HighCard r1) (HighCard r2) = compare r1 r2
  compare (Pair p1 k1) (Pair p2 k2) = compare p1 p2 <> compare k1 k2
  compare (TwoPair p1a p1b k1) (TwoPair p2a p2b k2) = compare p1a p2a <> compare p1b p2b <> compare k1 k2
  compare (ThreeOfAKind t1 k1) (ThreeOfAKind t2 k2) = compare t1 t2 <> compare k1 k2
  compare (Straight s1) (Straight s2) = compare s1 s2
  compare (Flush r1) (Flush r2) = compare r1 r2
  compare (FullHouse t1 p1) (FullHouse t2 p2) = compare t1 t2 <> compare p1 p2
  compare (FourOfAKind f1 k1) (FourOfAKind f2 k2) = compare f1 f2 <> compare k1 k2
  compare (StraightFlush s1) (StraightFlush s2) = compare s1 s2
  
  -- Compare different hand types
  compare (HighCard _) _ = LT
  compare _ (HighCard _) = GT
  compare (Pair _ _) _ = LT
  compare _ (Pair _ _) = GT
  compare (TwoPair _ _ _) _ = LT
  compare _ (TwoPair _ _ _) = GT
  compare (ThreeOfAKind _ _) _ = LT
  compare _ (ThreeOfAKind _ _) = GT
  compare (Straight _) _ = LT
  compare _ (Straight _) = GT
  compare (Flush _) _ = LT
  compare _ (Flush _) = GT
  compare (FullHouse _ _) _ = LT
  compare _ (FullHouse _ _) = GT
  compare (FourOfAKind _ _) _ = LT
  compare _ (FourOfAKind _ _) = GT

-- Parse a single card from string (e.g., "2H" for 2 of Hearts)
parseCard :: String -> Maybe Card
parseCard [r, s] = Card <$> parseRank r <*> parseSuit s
  where
    parseRank c = case c of
      '2' -> Just Two
      '3' -> Just Three
      '4' -> Just Four
      '5' -> Just Five
      '6' -> Just Six
      '7' -> Just Seven
      '8' -> Just Eight
      '9' -> Just Nine
      'T' -> Just Ten
      'J' -> Just Jack
      'Q' -> Just Queen
      'K' -> Just King
      'A' -> Just Ace
      _ -> Nothing
    
    parseSuit c = case c of
      'H' -> Just Hearts
      'D' -> Just Diamonds
      'C' -> Just Clubs
      'S' -> Just Spades
      _ -> Nothing
parseCard _ = Nothing

-- Parse a hand string into list of cards (e.g., "2H 3D 5S 9C KD")
parseHand :: String -> Maybe [Card]
parseHand s = traverse parseCard (words s)

-- Extract ranks and suits from cards
ranks :: [Card] -> [Rank]
ranks = map (\(Card r _) -> r)

suits :: [Card] -> [Suit]
suits = map (\(Card _ s) -> s)

-- Check if all elements are the same
allSame :: Eq a => [a] -> Bool
allSame xs = all (== head xs) (tail xs)

-- Check if hand is a flush (all same suit)
isFlush :: [Card] -> Bool
isFlush = allSame . suits

-- Check if hand is a straight (consecutive ranks)
isStraight :: [Rank] -> Bool
isStraight rs = let sorted = sort rs
                in (toEnum (fromEnum (head sorted) + 4) == last sorted) ||
                   (sorted == [Two, Three, Four, Five, Ace])  -- Special case: Ace-low straight

-- Get rank frequencies in descending order of frequency and rank
rankFrequencies :: [Rank] -> [(Int, Rank)]
rankFrequencies rs = sortBy (comparing (negate . fst) <> comparing (negate . snd)) $
                     map (\g -> (length g, head g)) (group (sort rs))

-- Rank a hand
rankHand :: [Card] -> HandRank
rankHand cards = case (isFlush cards, isStraight sortedRanks, rankFrequencies sortedRanks) of
  (True, True, _) -> if head sortedRanks == Ten then StraightFlush Ace else StraightFlush (last sortedRanks)
  (True, False, _) -> Flush sortedRanks
  (False, True, _) -> if head sortedRanks == Two then Straight Five else Straight (last sortedRanks)
  (False, False, (4, r1):(1, r2):_) -> FourOfAKind r1 r2
  (False, False, (3, r1):(2, r2):_) -> FullHouse r1 r2
  (False, False, (3, r1):(_, r2):(_, r3):_) -> ThreeOfAKind r1 [r2, r3]
  (False, False, (2, r1):(2, r2):(_, r3):_) -> TwoPair r1 r2 [r3]
  (False, False, (2, r1):(_, r2):(_, r3):(_, r4):_) -> Pair r1 [r2, r3, r4]
  _ -> HighCard sortedRanks
  where sortedRanks = sort ranks'
        ranks' = ranks cards

-- Main function to find the best hands
bestHands :: [String] -> Maybe [String]
bestHands hands = 
  let parsed = mapMaybe (\h -> (h,) <$> parseHand h) hands
      ranked = map (\(h, cards) -> (h, rankHand cards)) parsed
      bestRank = if null ranked then Nothing else Just (maximum (map snd ranked))
  in fmap (\r -> map fst (filter ((== r) . snd) ranked)) bestRank
