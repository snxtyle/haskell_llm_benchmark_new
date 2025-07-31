module Poker (bestHands) where

import Data.Char (toUpper)
import Data.List (group, sort, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.List as L
import qualified Data.Ord as O

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands =
  case mapMaybe parseAndRank hands of
    [] -> Nothing
    ranked ->
      let best = maximumBySnd ranked
          bestRank = snd best
          winners = [orig | (orig, r) <- ranked, r == bestRank]
      in Just winners

-- Utilities

maximumBySnd :: Ord b => [(a, b)] -> (a, b)
maximumBySnd = L.maximumBy (O.comparing snd)

-- Card and Hand parsing

data Suit = S | H | D | C deriving (Eq, Show)
type RankVal = Int -- 2..14 (14 is Ace)
data Card = Card { rank :: RankVal, suit :: Suit } deriving (Eq, Show)

-- Hand ranking representation: higher list lexicographically is better.
-- The first component is the category, higher is better.
-- Then tie-breaker ranks in descending order.
-- Categories:
-- 9 Straight Flush
-- 8 Four of a Kind
-- 7 Full House
-- 6 Flush
-- 5 Straight
-- 4 Three of a Kind
-- 3 Two Pairs
-- 2 One Pair
-- 1 High Card
type HandRank = (Int, [Int])

parseAndRank :: String -> Maybe (String, HandRank)
parseAndRank s = do
  cards <- parseHand s
  let r = rankHand cards
  pure (s, r)

parseHand :: String -> Maybe [Card]
parseHand s = do
  let parts = words s
  if length parts /= 5 then Nothing else mapM parseCard parts

parseCard :: String -> Maybe Card
parseCard str =
  case normalizeCard str of
    Just (r, su) -> Just (Card r su)
    Nothing -> Nothing

normalizeCard :: String -> Maybe (RankVal, Suit)
normalizeCard raw =
  case map toUpper raw of
    [r,su] -> (,) <$> parseRank [r] <*> parseSuit su
    ['1','0',su] -> (,) <$> parseRank "10" <*> parseSuit su
    -- Also accept 'T' for Ten commonly used in poker strings
    [r1,su] | r1 == 'T' -> (,) <$> parseRank "10" <*> parseSuit su
    _ -> Nothing

parseSuit :: Char -> Maybe Suit
parseSuit c =
  case c of
    'S' -> Just S
    'H' -> Just H
    'D' -> Just D
    'C' -> Just C
    _   -> Nothing

parseRank :: String -> Maybe RankVal
parseRank r =
  case r of
    "2" -> Just 2
    "3" -> Just 3
    "4" -> Just 4
    "5" -> Just 5
    "6" -> Just 6
    "7" -> Just 7
    "8" -> Just 8
    "9" -> Just 9
    "10" -> Just 10
    "J" -> Just 11
    "Q" -> Just 12
    "K" -> Just 13
    "A" -> Just 14
    _   -> Nothing

-- Ranking

rankHand :: [Card] -> HandRank
rankHand cards =
  let rs = sortBy (flip compare) (map rank cards) -- descending
      ss = map suit cards
      isFlush = all (== head ss) ss
      (isStraight, straightHigh) = straightInfo rs
      groups = sortBy groupOrder (map (\g -> (length g, head g)) (group (sort rs)))
      -- groups sorted by size desc, then rank desc
      -- E.g., Four of a kind: [(4,x),(1,y)]
      -- Full house: [(3,x),(2,y)]
      -- Three of a kind: [(3,x),(1,y),(1,z)]
      -- Two pairs: [(2,x),(2,y),(1,z)] with x>y because of groupOrder
      -- One pair: [(2,x),(1,y),(1,z),(1,w)]
      kickersDesc = computeKickers groups
  in case () of
      _ | isStraight && isFlush -> (9, [straightHigh])
        | isFourKind groups     -> let [(4,a),(1,b)] = groups in (8, [a,b])
        | isFullHouse groups    -> let [(3,a),(2,b)] = groups in (7, [a,b])
        | isFlush               -> (6, rs)
        | isStraight            -> (5, [straightHigh])
        | isThreeKind groups    -> let [(3,a),(1,b),(1,c)] = groups in (4, [a,b,c])
        | isTwoPairs groups     -> let [(2,a),(2,b),(1,c)] = groups in (3, [a,b,c])
        | isOnePair groups      -> let [(2,a),(1,b),(1,c),(1,d)] = groups in (2, [a,b,c,d])
        | otherwise             -> (1, rs)

groupOrder :: (Int, Int) -> (Int, Int) -> Ordering
groupOrder (c1, r1) (c2, r2) =
  compare c2 c1 <> compare r2 r1

isFourKind :: [(Int, Int)] -> Bool
isFourKind ((4,_):(1,_):[]) = True
isFourKind _                = False

isFullHouse :: [(Int, Int)] -> Bool
isFullHouse ((3,_):(2,_):[]) = True
isFullHouse _                = False

isThreeKind :: [(Int, Int)] -> Bool
isThreeKind ((3,_):(1,_):(1,_):[]) = True
isThreeKind _                      = False

isTwoPairs :: [(Int, Int)] -> Bool
isTwoPairs ((2,_):(2,_):(1,_):[]) = True
isTwoPairs _                      = False

isOnePair :: [(Int, Int)] -> Bool
isOnePair ((2,_):(1,_):(1,_):(1,_):[]) = True
isOnePair _                            = False

computeKickers :: [(Int, Int)] -> [Int]
computeKickers gs = concatMap expand gs
  where
    expand (cnt, r) = replicate cnt r

-- Straight handling (including low-A straight A-2-3-4-5)
straightInfo :: [Int] -> (Bool, Int)
straightInfo ranksDesc =
  let uniqDesc = reverse $ L.nub $ reverse ranksDesc -- keep order desc while removing duplicates
      -- normalize for A-2-3-4-5 straight: treat Ace as 1
      lowA = if 14 `elem` uniqDesc then map (\x -> if x == 14 then 1 else x) uniqDesc else uniqDesc
      isSeq xs = length xs == 5 && and (zipWith (\a b -> a - 1 == b) xs (tail xs))
      desc = uniqDesc
      descLow = sortBy (flip compare) lowA
  in if isSeq desc
        then (True, head desc)
     else if isSeq descLow
        then (True, head descLow)  -- in low straight, head will be 5
     else (False, 0)
