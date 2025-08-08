module Poker (bestHands) where

import Data.Char (digitToInt)
import Data.Function (on)
import Data.List (group, sort, sortBy)
import Data.Maybe (mapMaybe)

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands handStrs =
  case traverse (\s -> fmap (\cs -> (s, cs)) (parseHand s)) handStrs of
    Nothing -> Nothing
    Just parsed ->
      let scored = map (\(s, cs) -> (evaluateHand cs, s)) parsed
          maxScore = maximum (map fst scored)
          winners = [ s | (sc, s) <- scored, sc == maxScore ]
       in Just winners

-- Parsing

type Rank = Int
type Suit = Char
type Card = (Rank, Suit)

parseHand :: String -> Maybe [Card]
parseHand s =
  let toks = words s
   in if length toks /= 5
        then Nothing
        else traverse parseCard toks

parseCard :: String -> Maybe Card
parseCard str
  | length str < 2 = Nothing
  | otherwise =
      let suit = last str
          valStr = init str
       in do
            r <- parseRank valStr
            if suit `elem` ['S','H','D','C']
              then Just (r, suit)
              else Nothing

parseRank :: String -> Maybe Rank
parseRank "10" = Just 10
parseRank [c] =
  case c of
    'A' -> Just 14
    'K' -> Just 13
    'Q' -> Just 12
    'J' -> Just 11
    'T' -> Just 10
    d | d >= '2' && d <= '9' -> Just (digitToInt d)
    _ -> Nothing
parseRank _ = Nothing

-- Hand evaluation

data Category
  = High
  | Pair
  | TwoPair
  | Three
  | Straight
  | Flush
  | FullHouse
  | Four
  | StraightFlush
  deriving (Eq, Ord, Show, Enum, Bounded)

type Score = (Category, [Int])

evaluateHand :: [Card] -> Score
evaluateHand cards =
  let ranks = map fst cards
      suits = map snd cards
      rsAsc = sort ranks
      rsDesc = reverse rsAsc
      flush = isFlush suits
      mStraightHi = straightHigh rsAsc
      groupsAsc = group (sort ranks) -- [[rank,rank,...] ...]
      groupsSorted = sortBy compareGroups groupsAsc
      counts = map length groupsSorted
      flattened = concat groupsSorted
   in case () of
        _
          -- Straight Flush
          | flush, Just hi <- mStraightHi ->
              (StraightFlush, [hi])
          -- Four of a Kind
          | counts == [4,1] ->
              (Four, flattened)
          -- Full House
          | counts == [3,2] ->
              (FullHouse, flattened)
          -- Flush
          | flush ->
              (Flush, rsDesc)
          -- Straight
          | Just hi <- mStraightHi ->
              (Straight, [hi])
          -- Three of a Kind
          | counts == [3,1,1] ->
              (Three, flattened)
          -- Two Pair
          | counts == [2,2,1] ->
              (TwoPair, flattened)
          -- One Pair
          | counts == [2,1,1,1] ->
              (Pair, flattened)
          -- High Card
          | otherwise ->
              (High, rsDesc)

compareGroups :: [Int] -> [Int] -> Ordering
compareGroups a b =
  case compare (length b) (length a) of
    EQ -> compare (head b) (head a) -- higher rank first
    ord -> ord

isFlush :: [Suit] -> Bool
isFlush [] = False
isFlush (s:ss) = all (== s) ss

-- Returns Just highest card in the straight; treats A-2-3-4-5 as straight high 5.
straightHigh :: [Rank] -> Maybe Rank
straightHigh rsAsc =
  case rsAsc of
    [2,3,4,5,14] -> Just 5
    _ ->
      if isConsecutive rsAsc
        then Just (last rsAsc)
        else Nothing

isConsecutive :: [Int] -> Bool
isConsecutive xs =
  case xs of
    [_] -> True
    _ ->
      and (zipWith (\a b -> b - a == 1) xs (tail xs))
