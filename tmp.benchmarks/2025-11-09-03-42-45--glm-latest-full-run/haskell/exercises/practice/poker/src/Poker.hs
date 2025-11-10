module Poker (bestHands) where

import Data.List (groupBy, sortBy, sortOn, sort)
import Data.Function (on)
import Data.Ord (Down(..))

-- Data representation for cards, suits, and ranks
data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Ord, Show)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
          deriving (Eq, Ord, Show)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show)

-- Data representation for a hand's rank, including tie-breakers
-- The lists of ranks are always sorted in descending order for easy comparison.
data HandRank
  = HighCard [Rank]           -- All 5 kickers
  | Pair Rank [Rank]          -- Pair rank, then 3 kickers
  | TwoPair Rank Rank [Rank]  -- High pair, low pair, then 1 kicker
  | ThreeOfAKind Rank [Rank]  -- Trips rank, then 2 kickers
  | Straight Rank             -- High card of the straight
  | Flush [Rank]              -- All 5 card ranks, sorted
  | FullHouse Rank Rank       -- Trips rank, then pair rank
  | FourOfAKind Rank [Rank]   -- Quads rank, then 1 kicker
  | StraightFlush Rank        -- High card of the straight flush
  | RoyalFlush                -- Ten-to-Ace straight flush
  deriving (Eq, Show)

-- Ord instance for HandRank to compare hands according to poker rules.
instance Ord HandRank where
  compare RoyalFlush RoyalFlush = EQ
  compare RoyalFlush _ = GT
  compare _ RoyalFlush = LT

  compare (StraightFlush r1) (StraightFlush r2) = compare r1 r2
  compare (StraightFlush _) _ = GT
  compare _ (StraightFlush _) = LT

  compare (FourOfAKind r1 k1) (FourOfAKind r2 k2) = compare r1 r2 <> compare k1 k2
  compare (FourOfAKind _ _) _ = GT
  compare _ (FourOfAKind _ _) = LT

  compare (FullHouse t1 p1) (FullHouse t2 p2) = compare t1 t2 <> compare p1 p2
  compare (FullHouse _ _) _ = GT
  compare _ (FullHouse _ _) = LT

  compare (Flush r1) (Flush r2) = compare r1 r2
  compare (Flush _) _ = GT
  compare _ (Flush _) = LT

  compare (Straight r1) (Straight r2) = compare r1 r2
  compare (Straight _) _ = GT
  compare _ (Straight _) = LT

  compare (ThreeOfAKind r1 k1) (ThreeOfAKind r2 k2) = compare r1 r2 <> compare k1 k2
  compare (ThreeOfAKind _ _) _ = GT
  compare _ (ThreeOfAKind _ _) = LT

  compare (TwoPair hp1 lp1 k1) (TwoPair hp2 lp2 k2) = compare hp1 hp2 <> compare lp1 lp2 <> compare k1 k2
  compare (TwoPair _ _ _) _ = GT
  compare _ (TwoPair _ _ _) = LT

  compare (Pair r1 k1) (Pair r2 k2) = compare r1 r2 <> compare k1 k2
  compare (Pair _ _) _ = GT
  compare _ (Pair _ _) = LT

  compare (HighCard r1) (HighCard r2) = compare r1 r2

-- | Parses a single character into a Suit
parseSuit :: Char -> Suit
parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit 'S' = Spades
parseSuit c = error $ "Invalid suit: " ++ [c]

-- | Parses a single character into a Rank
parseRank :: Char -> Rank
parseRank '2' = Two
parseRank '3' = Three
parseRank '4' = Four
parseRank '5' = Five
parseRank '6' = Six
parseRank '7' = Seven
parseRank '8' = Eight
parseRank '9' = Nine
parseRank 'T' = Ten
parseRank 'J' = Jack
parseRank 'Q' = Queen
parseRank 'K' = King
parseRank 'A' = Ace
parseRank c = error $ "Invalid rank: " ++ [c]

-- | Parses a card string like "4S" into a Card
parseCard :: String -> Card
parseCard (r:s:[]) = Card { rank = parseRank r, suit = parseSuit s }
parseCard s = error $ "Invalid card string: " ++ s

-- | Parses a hand string like "4S 5H 6D 7C 8D" into a list of Cards
parseHand :: String -> [Card]
parseHand = map parseCard . words

-- | Groups cards by rank and counts them, sorted by rank descending.
groupRanks :: [Card] -> [(Rank, Int)]
groupRanks cards = map (\g -> (rank (head g), length g)) $
                   groupBy ((==) `on` rank) $
                   sortBy (compare `on` (Down . rank)) cards

-- | Checks if all cards have the same suit.
isFlush :: [Card] -> Bool
isFlush cards = all (== suit (head cards)) (map suit cards)

-- | Checks if the cards form a straight. Returns the high card rank if it's a straight.
isStraight :: [Card] -> Maybe Rank
isStraight cards
  -- Check for the special case: A-2-3-4-5 (wheel straight)
  | ranks == [Two, Three, Four, Five, Ace] = Just Five
  -- Check for a regular straight (e.g., 10-J-Q-K-A)
  | all (\(r1, r2) -> succ r1 == r2) (zip ranks (tail ranks)) = Just (last ranks)
  | otherwise = Nothing
  where
    ranks = sort $ map rank cards

-- | Evaluates a 5-card hand and determines its HandRank.
evaluateHand :: [Card] -> HandRank
evaluateHand cards =
  let sortedCards = sortBy (compare `on` (Down . rank)) cards
      ranks = map rank sortedCards
      groups = sortOn (Down . snd) $ groupRanks sortedCards
      isFlushHand = isFlush sortedCards
      maybeStraightHigh = isStraight sortedCards
  in case (isFlushHand, maybeStraightHigh) of
       (True, Just Ace) | ranks == [Ten, Jack, Queen, King, Ace] -> RoyalFlush
       (True, Just r)   -> StraightFlush r
       (_, Just r)       -> Straight r
       (True, _)         -> Flush ranks
       _ -> case groups of
              [(r,4), (k,1)]      -> FourOfAKind r [k]
              [(t,3), (p,2)]      -> FullHouse t p
              [(t,3), (k1,1), (k2,1)] -> ThreeOfAKind t [k1, k2]
              [(p1,2), (p2,2), (k,1)] -> TwoPair p1 p2 [k]
              [(p,2), (k1,1), (k2,1), (k3,1)] -> Pair p [k1, k2, k3]
              _ -> HighCard ranks

-- | Picks the best hand(s) from a list of poker hands.
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands =
  let evaluatedHands = zip hands $ map (evaluateHand . parseHand) hands
      maxRank = maximum $ map snd evaluatedHands
  in Just $ map fst $ filter (\h -> snd h == maxRank) evaluatedHands
