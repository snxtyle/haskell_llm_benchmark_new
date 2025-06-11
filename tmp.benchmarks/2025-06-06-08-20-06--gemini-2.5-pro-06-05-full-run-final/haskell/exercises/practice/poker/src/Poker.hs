module Poker (bestHands) where

import Data.List (group, nub, sort, sortBy, sortOn)
import Data.Ord (Down (..), comparing)
import Data.Semigroup ((<>))

data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Eq, Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded, Show)

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Eq, Show)

instance Ord Card where
  compare c1 c2 = compare (rank c1) (rank c2)

data HandRank
  = StraightFlush Rank -- High card rank
  | FourOfAKind Rank Rank -- Rank of four, rank of kicker
  | FullHouse Rank Rank -- Rank of three, rank of pair
  | Flush [Rank] -- Ranks of 5 cards, descending
  | Straight Rank -- High card rank
  | ThreeOfAKind Rank [Rank] -- Rank of three, kickers descending
  | TwoPair Rank Rank Rank -- High pair, low pair, kicker
  | OnePair Rank [Rank] -- Pair rank, kickers descending
  | HighCard [Rank] -- All 5 card ranks, descending
  deriving (Eq, Ord, Show)

bestHands :: [String] -> Maybe [String]
bestHands handStrings = do
  parsedHandsWithOrig <- traverse parseHandWithOrig handStrings
  let allCards = concatMap snd parsedHandsWithOrig
  if not (null allCards) && length allCards /= length (nub allCards)
    then Nothing -- Duplicate cards across hands
    else case parsedHandsWithOrig of
      [] -> Just []
      hands ->
        let evaluatedHands = map (\(orig, cards) -> (orig, evaluateHand cards)) hands
            maxRank = maximum $ map snd evaluatedHands
            best = filter (\(_, r) -> r == maxRank) evaluatedHands
         in Just $ map fst best

parseHandWithOrig :: String -> Maybe (String, [Card])
parseHandWithOrig s = do
  cards <- parseHand s
  return (s, cards)

parseHand :: String -> Maybe [Card]
parseHand s =
  let cardStrings = words s
   in if length cardStrings /= 5
        then Nothing
        else do
          cards <- traverse parseCard cardStrings
          if length (nub cards) /= 5 -- check for duplicate cards within a hand
            then Nothing
            else Just cards

parseCard :: String -> Maybe Card
parseCard s =
  if length s < 2
    then Nothing
    else
      let (r, s') = splitAt (length s - 1) s
       in case s' of
            [suitChar] -> Card <$> parseRank r <*> parseSuit suitChar
            _ -> Nothing

parseRank :: String -> Maybe Rank
parseRank "2" = Just Two
parseRank "3" = Just Three
parseRank "4" = Just Four
parseRank "5" = Just Five
parseRank "6" = Just Six
parseRank "7" = Just Seven
parseRank "8" = Just Eight
parseRank "9" = Just Nine
parseRank "T" = Just Ten
parseRank "10" = Just Ten
parseRank "J" = Just Jack
parseRank "Q" = Just Queen
parseRank "K" = Just King
parseRank "A" = Just Ace
parseRank _ = Nothing

parseSuit :: Char -> Maybe Suit
parseSuit 'S' = Just Spades
parseSuit 'H' = Just Hearts
parseSuit 'D' = Just Diamonds
parseSuit 'C' = Just Clubs
parseSuit _ = Nothing

evaluateHand :: [Card] -> HandRank
evaluateHand cards
  | isFlush && isStraight = StraightFlush straightHighRank
  | groupLengths == [4, 1] = FourOfAKind (head groupRanks) (groupRanks !! 1)
  | groupLengths == [3, 2] = FullHouse (head groupRanks) (groupRanks !! 1)
  | isFlush = Flush sortedRanksDesc
  | isStraight = Straight straightHighRank
  | groupLengths == [3, 1, 1] = ThreeOfAKind (head groupRanks) (tail groupRanks)
  | groupLengths == [2, 2, 1] = TwoPair (head groupRanks) (groupRanks !! 1) (groupRanks !! 2)
  | groupLengths == [2, 1, 1, 1] = OnePair (head groupRanks) (tail groupRanks)
  | otherwise = HighCard sortedRanksDesc
  where
    sortedRanksDesc = sortOn Down $ map rank cards
    cardSuits = map suit cards
    isFlush = all (== head cardSuits) (tail cardSuits)
    (isStraight, straightHighRank) = checkStraight sortedRanksDesc
    -- for grouping, sort ranks ascending to group, then sort groups
    -- by size desc, then by rank desc to handle tie-breaking correctly.
    rankGroups = sortBy (comparing (Down . length) <> comparing (Down . head)) $ group $ sort $ map rank cards
    groupLengths = map length rankGroups
    groupRanks = map head groupRanks

checkStraight :: [Rank] -> (Bool, Rank)
checkStraight ranks@([r1, _, _, _, _])
  | ranks == [Ace, Five, Four, Three, Two] = (True, Five) -- Ace-low straight
  | isConsecutive ranks = (True, r1)
  | otherwise = (False, r1)
  where
    isConsecutive rs = and $ zipWith (\a b -> fromEnum a == fromEnum b + 1) rs (tail rs)
checkStraight _ = (False, Ace) -- Should not happen with 5 cards
