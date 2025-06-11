{-# LANGUAGE TupleSections #-}
module Poker (bestHands) where

import           Data.Char       (toUpper)
import           Data.List       (group, nub, sort, sortBy)
import           Data.Ord        (comparing)

-- | Public API ---------------------------------------------------------------

-- | Return the best hand(s) among the supplied hands.
--   * The input is a list of 5-card hands, each hand encoded as a string in
--     which cards are separated by spaces, e.g.  "4S 5S 7H 8D JC".
--   * If the input contains malformed hands or duplicated cards *within a
--     hand* the result is Nothing.
--   * Otherwise the result is @Just winners@ where @winners@ contains every
--     hand (from the original list) whose score is maximal.  When several
--     hands tie for the win they are all returned, preserving the order in
--     which they appeared in the input.
bestHands :: [String] -> Maybe [String]
bestHands handStrs = do
  parsed <- traverse parseHand handStrs                 -- 1. parse & per-hand validation
  if null parsed                                        -- guard against empty input
     then Nothing
     else
       let scored  = zip handStrs (map scoreHand parsed) -- 2. score
           best    = maximum (map snd scored)            -- 3. highest score
           winners = [h | (h,s) <- scored, s == best]    -- 4. pick winners
       in Just winners

-- | Card representation ------------------------------------------------------

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
          deriving (Eq, Ord, Enum, Bounded, Show)

data Card = Card
  { cRank :: !Rank
  , cSuit :: !Char   -- ^ One of  H, D, C, S
  } deriving (Eq, Show)

type Hand = [Card]   -- always 5 elements

-- | Parsing ------------------------------------------------------------------

parseHand :: String -> Maybe Hand
parseHand s = do
  let tokens = words s
  if length tokens /= 5
     then Nothing
     else do
       cards <- traverse parseCard tokens
       if length cards /= length (nub cards) then Nothing else Just cards

parseCard :: String -> Maybe Card
parseCard str
  | length str < 2 = Nothing
  | otherwise      =
      let suitChar = toUpper (last str)
          rankStr  = map toUpper (init str)
      in Card <$> parseRank rankStr <*> parseSuit suitChar

parseSuit :: Char -> Maybe Char
parseSuit c
  | c `elem` suits = Just c
  | otherwise      = Nothing
  where suits = "HDSC"

parseRank :: String -> Maybe Rank
parseRank r = lookup r table
  where
    table = [("2",Two), ("3",Three), ("4",Four), ("5",Five), ("6",Six)
            ,("7",Seven), ("8",Eight), ("9",Nine), ("10",Ten), ("T",Ten)
            ,("J",Jack) ,("Q",Queen), ("K",King) ,("A",Ace)]

-- | Scoring ------------------------------------------------------------------

-- A score is a pair where the first element encodes the hand category
-- (higher is better) and the second element is a list of integers used for
-- tie-breaking.  Ordinary tuple comparison does exactly what we want.
type Score = (Int,[Int])

scoreHand :: Hand -> Score
scoreHand hand
  | isStraight && isFlush = (8, [straightHigh])
  | isFourOfKind          = (7, [quadRank, kicker])
  | isFullHouse           = (6, [tripleRank, pairRank])
  | isFlush               = (5, ranksDesc)
  | isStraight            = (4, [straightHigh])
  | isThreeOfKind         = (3, tripleRank : kickers)
  | isTwoPair             = (2, highPair : lowPair : kickers)
  | isOnePair             = (1, pairRank : kickers)
  | otherwise             = (0, ranksDesc)
  where
    ranks        = map (rankValue . cRank) hand
    suits        = map cSuit hand
    ranksDesc    = sortDesc ranks
    grouped      = sortBy (comparing (negate . length))    -- most frequent first
                  . sortBy (comparing head)                -- then by rank
                  . group . sort $ ranks

    counts       = map (\g -> (length g, head g)) grouped  -- [(cnt,rank)]
    countRanks n = [r | (c,r) <- counts, c == n]

    -- Helpers for specific hand categories --------------------------
    isFlush      = all (== head suits) suits

    isStraight   = let s = sort ranks
                       wheel = [2,3,4,5,14]               -- A-2-3-4-5
                   in (and $ zipWith (\a b -> b - a == 1) s (tail s))
                      || (sort ranks == wheel)

    straightHigh = if sort ranks == [2,3,4,5,14]
                      then 5   -- wheel straight is five-high
                      else maximum ranks

    isFourOfKind = not (null (countRanks 4))
    quadRank     = head (countRanks 4)
    kicker       = head (countRanks 1)

    isFullHouse  = not (null (countRanks 3)) && not (null (countRanks 2))
    tripleRank   = head (countRanks 3)
    pairRank     = head (countRanks 2)

    isThreeOfKind = not isFullHouse && not (null (countRanks 3))
    kickers       = sortDesc (countRanks 1)

    isTwoPair   = length (countRanks 2) == 2
    highPair    = maximum (countRanks 2)
    lowPair     = minimum (countRanks 2)

    isOnePair   = length (countRanks 2) == 1

-- | Utilities ----------------------------------------------------------------

rankValue :: Rank -> Int
rankValue r = fromEnum r + 2   -- Two = 2 … Ace = 14

sortDesc :: Ord a => [a] -> [a]
sortDesc = sortBy (flip compare)
