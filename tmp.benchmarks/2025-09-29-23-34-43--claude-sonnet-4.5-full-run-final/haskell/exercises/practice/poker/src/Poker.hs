module Poker (bestHands) where

import Data.List (group, sort, sortBy)
import Data.Ord (comparing, Down(..))

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq, Ord)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
  deriving (Eq, Ord, Enum, Bounded)

data Card = Card Rank Suit deriving (Eq)

data HandRank = HighCard [Rank]
              | OnePair Rank [Rank]
              | TwoPair Rank Rank Rank
              | ThreeOfAKind Rank [Rank]
              | Straight Rank
              | Flush [Rank]
              | FullHouse Rank Rank
              | FourOfAKind Rank Rank
              | StraightFlush Rank
              deriving (Eq, Ord)

bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands = 
  case traverse parseHand hands of
    Nothing -> Nothing
    Just parsedHands -> 
      let rankedHands = zip hands (map rankHand parsedHands)
          bestRank = maximum (map snd rankedHands)
          winners = map fst $ filter (\(_, rank) -> rank == bestRank) rankedHands
      in Just winners

parseHand :: String -> Maybe [Card]
parseHand s = traverse parseCard (words s)

parseCard :: String -> Maybe Card
parseCard [r, s] = Card <$> parseRank r <*> parseSuit s
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
parseRank _ = Nothing

parseSuit :: Char -> Maybe Suit
parseSuit 'S' = Just Spades
parseSuit 'H' = Just Hearts
parseSuit 'D' = Just Diamonds
parseSuit 'C' = Just Clubs
parseSuit _ = Nothing

rankHand :: [Card] -> HandRank
rankHand cards = 
  let ranks = sort [r | Card r _ <- cards]
      suits = [s | Card _ s <- cards]
      isFlush = case suits of
                  [] -> False
                  (s:ss) -> all (== s) ss
      isStraight = checkStraight ranks
      groupedRanks = sortBy (comparing (Down . length)) $ group $ sort ranks
      groupSizes = map length groupedRanks
      groupHeads = map (\g -> case g of (x:_) -> x; [] -> error "empty group") groupedRanks
  in case (isFlush, isStraight) of
       (True, Just highRank) -> StraightFlush highRank
       (False, Just highRank) -> Straight highRank
       (True, Nothing) -> Flush (sortBy (comparing Down) ranks)
       (False, Nothing) -> 
         case groupSizes of
           [4, 1] -> FourOfAKind (groupHeads !! 0) (groupHeads !! 1)
           [3, 2] -> FullHouse (groupHeads !! 0) (groupHeads !! 1)
           [3, 1, 1] -> ThreeOfAKind (groupHeads !! 0) (sortBy (comparing Down) (groupHeads !! 1 : [groupHeads !! 2]))
           [2, 2, 1] -> TwoPair (max (groupHeads !! 0) (groupHeads !! 1)) 
                                (min (groupHeads !! 0) (groupHeads !! 1))
                                (groupHeads !! 2)
           [2, 1, 1, 1] -> OnePair (groupHeads !! 0) (sortBy (comparing Down) (drop 1 groupHeads))
           _ -> HighCard (sortBy (comparing Down) ranks)

checkStraight :: [Rank] -> Maybe Rank
checkStraight ranks
  | isConsecutive ranks = Just (maximum ranks)
  | ranks == [Two, Three, Four, Five, Ace] = Just Five  -- Ace-low straight (wheel)
  | otherwise = Nothing
  where
    isConsecutive rs = 
      case sort rs of
        [] -> False
        sorted@(first:_) -> 
          let enumerated = take (length sorted) [first ..]
          in sorted == enumerated
