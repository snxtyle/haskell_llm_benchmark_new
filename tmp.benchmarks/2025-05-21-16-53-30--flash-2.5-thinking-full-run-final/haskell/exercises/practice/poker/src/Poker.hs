module Poker (bestHands) where

import Data.List (sort, sortBy, nub, maximumBy, groupBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)

-- Data Types
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Show, Eq, Ord, Enum)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Show, Eq, Ord, Enum)

data Card = Card { rank :: Rank, suit :: Suit } deriving (Show, Eq)

-- Custom Ord instance for Card to sort by rank primarily, then suit (suit is secondary for tie-breaking, but not critical for hand evaluation beyond flush)
instance Ord Card where
    compare (Card r1 s1) (Card r2 s2) =
        case compare r1 r2 of
            EQ -> compare s1 s2
            x  -> x

-- HandType represents the type of poker hand, with information for tie-breaking
data HandType
    = HighCard [Rank]           -- All 5 card ranks, sorted descending
    | Pair Rank [Rank]          -- Pair rank, then remaining 3 kickers sorted descending
    | TwoPair Rank Rank [Rank]  -- Higher pair rank, lower pair rank, then kicker (sorted descending)
    | ThreeOfAKind Rank [Rank]  -- Trips rank, then remaining 2 kickers sorted descending
    | Straight Rank             -- Highest card in the straight
    | Flush [Rank]              -- All 5 card ranks, sorted descending
    | FullHouse Rank Rank       -- Trips rank, then pair rank
    | FourOfAKind Rank [Rank]   -- Quads rank, then kicker (sorted descending)
    | StraightFlush Rank        -- Highest card in the straight flush
    | RoyalFlush                -- No tie-breaking needed
    deriving (Show, Eq)

-- Ord instance for HandType to compare hands. Order is from highest poker hand to lowest.
instance Ord HandType where
    compare h1 h2 =
        case (h1, h2) of
            (RoyalFlush, RoyalFlush) -> EQ
            (RoyalFlush, _)          -> GT
            (_, RoyalFlush)          -> LT

            (StraightFlush r1, StraightFlush r2) -> compare r1 r2
            (StraightFlush _, _)                 -> GT
            (_, StraightFlush _)                 -> LT

            (FourOfAKind r1 k1, FourOfAKind r2 k2) -> compare (r1, k1) (r2, k2)
            (FourOfAKind _ _, _)                     -> GT
            (_, FourOfAKind _ _)                     -> LT

            (FullHouse t1 p1, FullHouse t2 p2) -> compare (t1, p1) (t2, p2)
            (FullHouse _ _, _)                   -> GT
            (_, FullHouse _ _)                   -> LT

            (Flush rs1, Flush rs2) -> compare rs1 rs2
            (Flush _, _)           -> GT
            (_, Flush _)           -> LT

            (Straight r1, Straight r2) -> compare r1 r2
            (Straight _, _)            -> GT
            (_, Straight _)            -> LT

            (ThreeOfAKind t1 ks1, ThreeOfAKind t2 ks2) -> compare (t1, ks1) (t2, ks2)
            (ThreeOfAKind _ _, _)                        -> GT
            (_, ThreeOfAKind _ _)                        -> LT

            (TwoPair hp1 lp1 k1, TwoPair hp2 lp2 k2) -> compare (hp1, lp1, k1) (hp2, lp2, k2)
            (TwoPair _ _ _, _)                        -> GT
            (_, TwoPair _ _ _, _)                     -> LT

            (Pair p1 ks1, Pair p2 ks2) -> compare (p1, ks1) (p2, ks2)
            (Pair _ _, _)                -> GT
            (_, Pair _ _)                -> LT

            (HighCard rs1, HighCard rs2) -> compare rs1 rs2
            -- All other hand types are greater than HighCard.
            (HighCard _, _) -> LT
            (_, HighCard _) -> GT


-- Represents an evaluated hand: original string, parsed cards, and its type
data EvaluatedHand = EvaluatedHand
    { originalString :: String
    , cards          :: [Card]
    , handType       :: HandType
    } deriving (Show, Eq)

-- Ord instance for EvaluatedHand to compare based on handType
instance Ord EvaluatedHand where
    compare eh1 eh2 = compare (handType eh1) (handType eh2)

-- Parsing functions
parseCard :: String -> Maybe Card
parseCard [rChar, sChar] = do
    rank' <- parseRank rChar
    suit' <- parseSuit sChar
    return $ Card rank' suit'
parseCard _ = Nothing

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
parseSuit 'C' = Just Clubs
parseSuit 'D' = Just Diamonds
parseSuit 'H' = Just Hearts
parseSuit 'S' = Just Spades
parseSuit _   = Nothing

parseHand :: String -> Maybe [Card]
parseHand s =
    let cardStrings = words s
    in if length cardStrings == 5
       then mapM parseCard cardStrings
       else Nothing

-- Helper to count ranks using Data.List
-- Returns a list of (Rank, Count) tuples, sorted by Rank ascending initially.
countRanks :: [Rank] -> [(Rank, Int)]
countRanks = map (\xs -> (head xs, length xs)) . groupBy (==) . sort

-- Hand evaluation logic
evaluateHand :: [Card] -> HandType
evaluateHand cs =
    let sortedCards = sortBy (flip compare) cs -- Sort cards descending by rank
        ranks = map rank sortedCards
        suits = map suit sortedCards
        isFlush = all (== head suits) (tail suits)
        isStraight = checkStraight ranks

        -- Count occurrences of each rank using Data.List
        -- Sort by count descending, then rank descending for tie-breaking within counts.
        sortedRankCounts = sortBy (\(r1, c1) (r2, c2) -> compare c2 c1 `mappend` compare r2 r1) $ countRanks ranks

        -- Extract ranks based on counts. These lists will already be sorted descending by rank.
        getRankByCount count = map fst . filter ((== count) . snd) $ sortedRankCounts
        quadsRanks = getRankByCount 4
        tripsRanks = getRankByCount 3
        pairRanks = getRankByCount 2
        singleRanks = getRankByCount 1

    in case (isFlush, isStraight) of
        (True, True) ->
            if ranks == [Ace, King, Queen, Jack, Ten] -- Check for Royal Flush (A, K, Q, J, T)
            then RoyalFlush
            else StraightFlush (head ranks)
        (False, True) -> Straight (head ranks)
        (True, False) -> Flush ranks -- ranks are already sorted descending
        (False, False) ->
            case sortedRankCounts of
                -- Four of a Kind: one group of 4, one group of 1
                ((r4,4) : (r1,1) : []) -> FourOfAKind r4 singleRanks
                -- Full House: one group of 3, one group of 2
                ((r3,3) : (r2,2) : []) -> FullHouse r3 r2
                -- Three of a Kind: one group of 3, two groups of 1
                ((r3,3) : (s1,1) : (s2,1) : []) -> ThreeOfAKind r3 singleRanks
                -- Two Pair: two groups of 2, one group of 1
                ((p1,2) : (p2,2) : (s1,1) : []) -> TwoPair (head pairRanks) (last pairRanks) singleRanks
                -- One Pair: one group of 2, three groups of 1
                ((p1,2) : (s1,1) : (s2,1) : (s3,1) : []) -> Pair (head pairRanks) singleRanks
                -- High Card: five groups of 1
                _ -> HighCard ranks -- ranks are already sorted descending

-- Helper for checking straight, considering Ace low (A,2,3,4,5)
checkStraight :: [Rank] -> Bool
checkStraight rs =
    let sortedUniqueRanks = sort . nub $ rs
        -- Standard straight check
        isStdStraight = length sortedUniqueRanks == 5 &&
                        (last sortedUniqueRanks == head sortedUniqueRanks `plusRank` 4)
        -- Ace-low straight (A,2,3,4,5)
        isAceLowStraight = sortedUniqueRanks == [Two, Three, Four, Five, Ace]
    in isStdStraight || isAceLowStraight

-- Helper to add to a rank (for straight check)
plusRank :: Rank -> Int -> Rank
plusRank r n = toEnum $ fromEnum r + n

-- Main function: Picks the best hand(s) from a list of poker hand strings.
bestHands :: [String] -> Maybe [String]
bestHands handStrings =
    let
        -- Parse and evaluate all hands, keeping track of original strings.
        -- Invalid hands (parsing errors) are filtered out.
        evaluatedHands = catMaybes $ map (\s -> fmap (\cs -> EvaluatedHand s cs (evaluateHand cs)) (parseHand s)) handStrings
    in
        if null evaluatedHands
        then Nothing -- No valid hands to evaluate
        else
            let
                -- Find the best hand type among all evaluated hands
                bestHandType = handType $ maximumBy (comparing handType) evaluatedHands
                -- Filter for all hands that match the best hand type (for ties)
                winningHands = filter (\eh -> handType eh == bestHandType) evaluatedHands
            in
                Just $ map originalString winningHands -- Return the original strings of the winning hands
