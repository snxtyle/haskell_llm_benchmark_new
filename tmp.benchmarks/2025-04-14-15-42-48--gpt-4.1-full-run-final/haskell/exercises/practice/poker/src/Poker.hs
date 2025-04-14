module Poker (bestHands) where

import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map.Strict as Map

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Show, Ord)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
    deriving (Eq, Ord, Enum, Bounded, Show)

type Card = (Rank, Suit)
type Hand = [Card]

data HandRank
    = HighCard [Rank]
    | OnePair Rank [Rank]
    | TwoPair Rank Rank [Rank]
    | ThreeOfAKind Rank [Rank]
    | Straight Rank
    | Flush [Rank]
    | FullHouse Rank Rank
    | FourOfAKind Rank [Rank]
    | StraightFlush Rank
    deriving (Eq, Show, Ord)

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands =
    let parsed = map parseHand hands
        ranked = zip hands (map handRank parsed)
        best = maximumByMay (comparing snd) ranked
        bestRank = fmap snd best
        winners = [h | (h, r) <- ranked, Just r == bestRank]
    in if null winners then Nothing else Just winners

-- Parse a hand from a string
parseHand :: String -> Hand
parseHand = map parseCard . words

parseCard :: String -> Card
parseCard [r, s] = (parseRank [r], parseSuit s)
parseCard [r1, r2, s] = (parseRank [r1, r2], parseSuit s)
parseCard _ = error "Invalid card format"

parseRank :: String -> Rank
parseRank "2" = Two
parseRank "3" = Three
parseRank "4" = Four
parseRank "5" = Five
parseRank "6" = Six
parseRank "7" = Seven
parseRank "8" = Eight
parseRank "9" = Nine
parseRank "10" = Ten
parseRank "J" = Jack
parseRank "Q" = Queen
parseRank "K" = King
parseRank "A" = Ace
parseRank _ = error "Invalid rank"

parseSuit :: Char -> Suit
parseSuit 'H' = Hearts
parseSuit 'D' = Diamonds
parseSuit 'C' = Clubs
parseSuit 'S' = Spades
parseSuit _ = error "Invalid suit"

-- Evaluate the hand and return its rank
handRank :: Hand -> HandRank
handRank hand
    | isStraightFlush = StraightFlush highStraight
    | isFourKind      = FourOfAKind fourKindRank kicker
    | isFullHouse     = FullHouse threeKindRank pairRank
    | isFlush         = Flush ranksDesc
    | isStraight      = Straight highStraight
    | isThreeKind     = ThreeOfAKind threeKindRank kickers
    | isTwoPair       = TwoPair highPair lowPair kicker
    | isOnePair       = OnePair pairRank kickers
    | otherwise       = HighCard ranksDesc
  where
    ranks = sortBy (flip compare) $ map fst hand
    suits = map snd hand
    rankGroups = sortBy (\a b -> compare (negate $ length a) (negate $ length b) <> compare (head b) (head a)) . group . sort $ map fst hand
    counts = map length rankGroups
    uniqueRanks = map head rankGroups
    ranksDesc = sortBy (flip compare) $ map fst hand

    isFlush = all (== head suits) suits
    isStraight = isJust straightHigh
    isStraightFlush = isFlush && isStraight
    straightHigh = getStraightHigh (map fst hand)
    highStraight = fromJust straightHigh

    isFourKind = counts == [4,1]
    fourKindRank = head (head rankGroups)
    kicker = [head (rankGroups !! 1)]

    isFullHouse = counts == [3,2]
    threeKindRank = head (head rankGroups)
    pairRank = head (rankGroups !! 1)

    isThreeKind = counts == [3,1,1]
    kickers = map head (drop 1 rankGroups)

    isTwoPair = counts == [2,2,1]
    highPair = max (head (head rankGroups)) (head (rankGroups !! 1))
    lowPair = min (head (head rankGroups)) (head (rankGroups !! 1))
    kicker = [head (rankGroups !! 2)]

    isOnePair = counts == [2,1,1,1]
    pairRank = head (head rankGroups)
    kickers = map head (drop 1 rankGroups)

-- Returns the high card of a straight, if present
getStraightHigh :: [Rank] -> Maybe Rank
getStraightHigh ranks =
    let rs = sort $ nub ranks
        aceLow = [Ace, Five, Four, Three, Two]
        allStraights = map (\r -> take 5 $ iterate pred r) [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five]
    in case find (`isSubsequenceOf` rs) allStraights of
        Just straight -> Just (head straight)
        Nothing -> if all (`elem` rs) aceLow then Just Five else Nothing

-- Safe maximumBy
maximumByMay :: (a -> a -> Ordering) -> [a] -> Maybe a
maximumByMay _ [] = Nothing
maximumByMay cmp xs = Just (maximumBy cmp xs)
