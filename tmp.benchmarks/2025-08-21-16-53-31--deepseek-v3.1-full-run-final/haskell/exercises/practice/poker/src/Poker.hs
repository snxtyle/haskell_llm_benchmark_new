module Poker (bestHands) where

import Data.List (sort, sortBy, group, maximumBy)
import Data.Ord (comparing)
import Data.Char (isDigit)

data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten 
          | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show)

data HandRank = HighCard [Rank] 
              | OnePair Rank [Rank] 
              | TwoPair Rank Rank Rank 
              | ThreeOfAKind Rank [Rank] 
              | Straight Rank 
              | Flush [Rank] 
              | FullHouse Rank Rank 
              | FourOfAKind Rank Rank 
              | StraightFlush Rank 
              deriving (Eq, Show)

instance Ord HandRank where
    compare hr1 hr2 = case (hr1, hr2) of
        (StraightFlush r1, StraightFlush r2) -> compare r1 r2
        (StraightFlush _, _) -> GT
        (_, StraightFlush _) -> LT
        
        (FourOfAKind r1 k1, FourOfAKind r2 k2) -> 
            case compare r1 r2 of
                EQ -> compare k1 k2
                other -> other
        (FourOfAKind {}, _) -> GT
        (_, FourOfAKind {}) -> LT
        
        (FullHouse t1 p1, FullHouse t2 p2) -> 
            case compare t1 t2 of
                EQ -> compare p1 p2
                other -> other
        (FullHouse {}, _) -> GT
        (_, FullHouse {}) -> LT
        
        (Flush rs1, Flush rs2) -> compare rs1 rs2
        (Flush {}, _) -> GT
        (_, Flush {}) -> LT
        
        (Straight r1, Straight r2) -> compare r1 r2
        (Straight {}, _) -> GT
        (_, Straight {}) -> LT
        
        (ThreeOfAKind r1 ks1, ThreeOfAKind r2 ks2) -> 
            case compare r1 r2 of
                EQ -> compare ks1 ks2
                other -> other
        (ThreeOfAKind {}, _) -> GT
        (_, ThreeOfAKind {}) -> LT
        
        (TwoPair h1 l1 k1, TwoPair h2 l2 k2) -> 
            case compare h1 h2 of
                EQ -> case compare l1 l2 of
                    EQ -> compare k1 k2
                    other -> other
                other -> other
        (TwoPair {}, _) -> GT
        (_, TwoPair {}) -> LT
        
        (OnePair r1 ks1, OnePair r2 ks2) -> 
            case compare r1 r2 of
                EQ -> compare ks1 ks2
                other -> other
        (OnePair {}, _) -> GT
        (_, OnePair {}) -> LT
        
        (HighCard rs1, HighCard rs2) -> compare rs1 rs2

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands = 
    let parsedHands = map parseHand hands
        evaluatedHands = map evaluateHand parsedHands
        maxHand = maximum evaluatedHands
        bestIndices = findIndices (== maxHand) evaluatedHands
    in Just (map (hands !!) bestIndices)

parseHand :: String -> [Card]
parseHand handStr = map parseCard (words handStr)

parseCard :: String -> Card
parseCard [r, s] = Card (parseRank r) (parseSuit s)
parseCard "10S" = Card Ten Spades
parseCard "10H" = Card Ten Hearts
parseCard "10D" = Card Ten Diamonds
parseCard "10C" = Card Ten Clubs
parseCard str = case span isDigit str of
    ("10", [s]) -> Card Ten (parseSuit s)
    _ -> error $ "Invalid card: " ++ str

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
parseRank r = error $ "Invalid rank: " ++ [r]

parseSuit :: Char -> Suit
parseSuit 'C' = Clubs
parseSuit 'D' = Diamonds
parseSuit 'H' = Hearts
parseSuit 'S' = Spades
parseSuit s = error $ "Invalid suit: " ++ [s]

evaluateHand :: [Card] -> HandRank
evaluateHand cards =
    let sortedCards = sortBy (comparing rank) cards
        ranks = map rank sortedCards
        suits = map suit sortedCards
        rankGroups = group $ sort ranks
        sortedGroups = sortBy (comparing length) rankGroups
        isFlush = all (== head suits) suits
        isStraight = isStraightHand ranks
    in case (isFlush, isStraight, sortedGroups) of
        (True, True, _) -> StraightFlush (last ranks)
        (_, _, groups) | length (last groups) == 4 -> 
            FourOfAKind (head $ last groups) (head $ head groups)
        (_, _, groups) | length (last groups) == 3 && length (head groups) == 2 -> 
            FullHouse (head $ last groups) (head $ head groups)
        (True, _, _) -> Flush (reverse ranks)
        (_, True, _) -> Straight (last ranks)
        (_, _, groups) | length (last groups) == 3 -> 
            ThreeOfAKind (head $ last groups) (getKickers ranks [head $ last groups])
        (_, _, groups) | length groups >= 2 && length (last groups) == 2 && length (last (init groups)) == 2 -> 
            let pairRanks = map head [last groups, last (init groups)]
                kicker = head $ head groups
            in TwoPair (maximum pairRanks) (minimum pairRanks) kicker
        (_, _, groups) | length (last groups) == 2 -> 
            OnePair (head $ last groups) (getKickers ranks [head $ last groups])
        _ -> HighCard (reverse ranks)

isStraightHand :: [Rank] -> Bool
isStraightHand ranks =
    let sorted = sort ranks
        isNormalStraight = all (== 1) $ zipWith (\x y -> fromEnum y - fromEnum x) sorted (tail sorted)
        isAceLowStraight = sorted == [Two, Three, Four, Five, Ace]
    in isNormalStraight || isAceLowStraight

getKickers :: [Rank] -> [Rank] -> [Rank]
getKickers allRanks excludeRanks = 
    let sorted = reverse $ sort allRanks
        filtered = filter (`notElem` excludeRanks) sorted
    in take (5 - length excludeRanks) filtered

findIndices :: Eq a => a -> [a] -> [Int]
findIndices x xs = [i | (i, y) <- zip [0..] xs, y == x]
