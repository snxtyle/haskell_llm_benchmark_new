module Poker (bestHands) where

import Data.Char (toUpper)
import Data.List (group, nub, sort, sortOn, words)
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))

-- Public API

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands handStrs = do
  parsed <- traverse parseHandWithOriginal handStrs
  let ranked = [ (handRank h, s) | (s, h) <- parsed ]
      best   = maximum (map fst ranked)
      winnersInOrder = [ s | (r, s) <- ranked, r == best ]
  pure winnersInOrder

-- Internal types

data Category
  = HighCard
  | OnePair
  | TwoPair
  | ThreeKind
  | Straight
  | Flush
  | FullHouse
  | FourKind
  | StraightFlush
  deriving (Eq, Ord, Show, Enum, Bounded)

data Card = Card { cRank :: Int, cSuit :: Char }
  deriving (Eq, Show)

type Hand = [Card]

type HandRank = (Category, [Int]) -- category and tie-breaker vector (lexicographic)

-- Parsing

parseHandWithOriginal :: String -> Maybe (String, Hand)
parseHandWithOriginal s = do
  h <- parseHand s
  pure (s, h)

parseHand :: String -> Maybe Hand
parseHand s = do
  let toks = words s
  -- exactly 5 cards
  if length toks /= 5
    then Nothing
    else do
      cards <- traverse parseCard toks
      -- no duplicate cards within a hand
      if length (nub cards) /= 5
        then Nothing
        else pure cards

parseCard :: String -> Maybe Card
parseCard tokRaw =
  let tok = map toUpper tokRaw
  in case tok of
      []      -> Nothing
      [_]     -> Nothing
      _       ->
        let rStr = init tok
            sChr = last tok
        in do
          r <- parseRank rStr
          s <- parseSuit sChr
          pure (Card r s)

parseRank :: String -> Maybe Int
parseRank r = case r of
  "A"  -> Just 14
  "K"  -> Just 13
  "Q"  -> Just 12
  "J"  -> Just 11
  "T"  -> Just 10
  _    -> case reads r :: [(Int, String)] of
            [(n, "")] | n >= 2 && n <= 10 -> Just n
            _                              -> Nothing

parseSuit :: Char -> Maybe Char
parseSuit c =
  let u = toUpper c
  in if u `elem` ['H','D','C','S'] then Just u else Nothing

-- Hand evaluation

handRank :: Hand -> HandRank
handRank hand =
  let rs      = map cRank hand
      rsDesc  = sortOn Down rs
      suits   = map cSuit hand
      isFlsh  = length (nub suits) == 1
      mStrHi  = straightHigh rs
      -- counts of ranks: list of (count, rank) sorted by count desc, then rank desc
      countsSorted :: [(Int, Int)]
      countsSorted =
        let grouped = group (sort rs) -- [[ranks equal], ...]
            asPairs = [ (length g, head g) | g <- grouped ]
        in sortOn (\(c, r) -> (Down c, Down r)) asPairs

      countsList = map fst countsSorted
      groupedRanksVector = concat [ replicate c r | (c, r) <- countsSorted ]

  in case (isFlsh, mStrHi, countsList) of
      (True, Just hi, _)        -> (StraightFlush, [hi])
      (_, _, [4,1])             -> (FourKind, groupedRanksVector)
      (_, _, [3,2])             -> (FullHouse, groupedRanksVector)
      (True, Nothing, _)        -> (Flush, rsDesc)
      (False, Just hi, _)       -> (Straight, [hi])
      (_, _, [3,1,1])           -> (ThreeKind, groupedRanksVector)
      (_, _, [2,2,1])           -> (TwoPair, groupedRanksVector)
      (_, _, [2,1,1,1])         -> (OnePair, groupedRanksVector)
      _                         -> (HighCard, rsDesc)

straightHigh :: [Int] -> Maybe Int
straightHigh ranks =
  let uniq = sort (nub ranks)
  in if length uniq /= 5
       then Nothing
       else
         -- handle Wheel: A-2-3-4-5
         if uniq == [2,3,4,5,14]
           then Just 5
           else if isConsecutive uniq
                  then Just (last uniq)
                  else Nothing

isConsecutive :: [Int] -> Bool
isConsecutive xs =
  and [ b - a == 1 | (a, b) <- zip xs (tail xs) ]
