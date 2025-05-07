{-# LANGUAGE TupleSections #-}
module Poker (bestHands) where

import Data.Char (toUpper)
import Data.List (group, sort, sortBy, sortOn)
import Data.Ord  (Down (..), comparing)
import qualified Data.Set as S

-- | Public API ─────────────────────────────────────────────────────────────

-- | Given a list of textual poker hands, return the best hand(s).
--   If the input is empty or any hand is invalid, return Nothing.
--
--   A hand is invalid when:
--     • it does not contain exactly five distinct cards
--     • any card’s rank / suit is unknown
bestHands :: [String] -> Maybe [String]
bestHands []   = Nothing
bestHands hs   =
  case traverse parseHand hs of
    Nothing      -> Nothing
    Just parsed  ->
      let ranked               = zip hs (map handRank parsed)
          (_, bestRank)        = maximumBySnd ranked
          winners              = map fst $ filter ((== bestRank) . snd) ranked
      in Just winners

-- | Helpers ────────────────────────────────────────────────────────────────

-- Safe variant of maximumBy on the 2‑tuple’s second component
maximumBySnd :: Ord b => [(a,b)] -> (a,b)
maximumBySnd = foldl1 maxBy
  where
    maxBy x@(_,rx) y@(_,ry)
      | ry > rx   = y
      | otherwise = x

-- | Parsing ────────────────────────────────────────────────────────────────

data Suit = Hearts | Clubs | Spades | Diamonds
  deriving (Eq, Ord, Show)

data Card = Card
  { rank :: Int   -- 2 .. 14  (where Ace == 14)
  , suit :: Suit
  } deriving (Eq, Ord, Show)

type Hand = [Card]  -- always five cards (after validation)

parseHand :: String -> Maybe Hand
parseHand t =
  let parts = words t
  in do
    guard (length parts == 5)
    cards <- traverse parseCard parts
    guard (S.size (S.fromList cards) == 5)      -- no duplicates
    pure cards
  where
    guard True  = Just ()
    guard False = Nothing

parseCard :: String -> Maybe Card
parseCard str =
  case fmap toUpper str of
    [r,s]               -> Card <$> parseRank r <*> parseSuit s
    ['1','0',s]         -> Card <$> pure 10        <*> parseSuit s
    _                   -> Nothing

parseRank :: Char -> Maybe Int
parseRank c = case c of
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  'T' -> Just 10
  'J' -> Just 11
  'Q' -> Just 12
  'K' -> Just 13
  'A' -> Just 14
  _   -> Nothing

parseSuit :: Char -> Maybe Suit
parseSuit c = case c of
  'H' -> Just Hearts
  'C' -> Just Clubs
  'S' -> Just Spades
  'D' -> Just Diamonds
  _   -> Nothing

-- | Hand ranking ───────────────────────────────────────────────────────────

-- Order matters!  The derived Ord instance gives ascending strength:
-- HighCard < OnePair < … < StraightFlush.
data Category
  = HighCard
  | OnePair
  | TwoPairs
  | ThreeKind
  | Straight
  | Flush
  | FullHouse
  | FourKind
  | StraightFlush
  deriving (Eq, Ord, Enum, Bounded, Show)

data HandRank = HandRank Category [Int]
  deriving (Eq, Show)

instance Ord HandRank where
  compare (HandRank c1 xs) (HandRank c2 ys) =
    compare c1 c2 <> compare xs ys

handRank :: Hand -> HandRank
handRank cards =
  let rs        = sortOn Down $ map rank cards      -- high → low
      isFlush   = all ((== suit (head cards)) . suit) cards
      (isStr, highStraight) = straightHigh rs
      grouped   = sortBy (compareCount `mappend` compareRank) $
                    map (\g -> (length g, head g)) $
                    group (sort rs)
      counts    = map fst grouped
      ranksByCount = map snd grouped
  in case () of
      _ | isStr && isFlush ->
            HandRank StraightFlush [highStraight]
        | counts == [4,1] ->
            HandRank FourKind (ranksByCount ++ kickers rs ranksByCount)
        | counts == [3,2] ->
            HandRank FullHouse ranksByCount
        | isFlush ->
            HandRank Flush rs
        | isStr ->
            HandRank Straight [highStraight]
        | counts == [3,1,1] ->
            HandRank ThreeKind (ranksByCount ++ kickers rs ranksByCount)
        | counts == [2,2,1] ->
            let [p1,p2,k] = ranksByCount ++ kickers rs ranksByCount
            in HandRank TwoPairs [p1,p2,k]
        | counts == [2,1,1,1] ->
            HandRank OnePair (ranksByCount ++ kickers rs ranksByCount)
        | otherwise ->
            HandRank HighCard rs
  where
    -- Comparison helpers for grouping
    compareCount = flip (comparing fst)  -- more cards first
    compareRank  = flip (comparing snd)  -- higher rank first

    -- Ranks not part of the primary grouping (used as kickers)
    kickers :: [Int] -> [Int] -> [Int]
    kickers allRanks main = filter (`notElem` main) allRanks

-- | Detect straight, returning (isStraight, highestCard)
--   Handles the special low‑Ace straight (A‑2‑3‑4‑5) where Ace counts as 1.
straightHigh :: [Int] -> (Bool, Int)
straightHigh rsHighToLow =
  let sorted = reverse (sort rsHighToLow)  -- lowest → highest
      uniq   = reverse $ sort $ rsHighToLow  -- keep order high → low, duplicates removed earlier
      isSeq xs = and $ zipWith (\a b -> a + 1 == b) xs (tail xs)
  in case uniq of
       [14,5,4,3,2] -> (True, 5)   -- Wheel straight (A‑2‑3‑4‑5)
       _ | length uniq == 5
         , isSeq (reverse uniq)    -- ascending check
         -> (True, maximum uniq)
       _ -> (False, 0)

