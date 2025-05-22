module Poker (bestHands) where

import Data.List (group, sort, sortBy)
import Data.Ord (comparing, Down(..))

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Eq, Ord, Show)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace 
          deriving (Eq, Ord, Show, Enum)

data Card = Card Rank Suit deriving (Eq, Show)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush
              deriving (Eq, Ord, Show)

type Hand = [Card]

bestHands :: [String] -> [String]
bestHands [] = []
bestHands hands = 
  case mapM parseHand hands of
    Nothing -> []
    Just parsedHands -> 
      let rankedHands = zip hands (map rankHand parsedHands)
          bestRank = maximum (map snd rankedHands)
          bestHandStrings = [hand | (hand, rank) <- rankedHands, rank == bestRank]
      in bestHandStrings

parseHand :: String -> Maybe Hand
parseHand handStr = mapM parseCard (words handStr)

parseCard :: String -> Maybe Card
parseCard [r, s] = do
  rank <- parseRank r
  suit <- parseSuit s
  return $ Card rank suit
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
parseSuit 'H' = Just Hearts
parseSuit 'D' = Just Diamonds
parseSuit 'C' = Just Clubs
parseSuit 'S' = Just Spades
parseSuit _ = Nothing

rankHand :: Hand -> (HandType, [Rank])
rankHand hand = 
  let ranks = sort [r | Card r _ <- hand]
      suits = [s | Card _ s <- hand]
      rankCounts = map length $ group $ sort ranks
      isFlush = length (group $ sort suits) == 1
      isStraight = isStraightSequence ranks
      
      handType = case (isStraight, isFlush, sort rankCounts) of
        (True, True, _) -> StraightFlush
        (_, _, [1,4]) -> FourOfAKind
        (_, _, [2,3]) -> FullHouse
        (_, True, _) -> Flush
        (True, _, _) -> Straight
        (_, _, [1,1,3]) -> ThreeOfAKind
        (_, _, [1,2,2]) -> TwoPair
        (_, _, [1,1,1,2]) -> OnePair
        _ -> HighCard
      
      -- For tie-breaking, we need ranks in order of importance
      tieBreakers = case handType of
        FourOfAKind -> let grouped = sortBy (comparing (Down . length)) $ group $ sort ranks
                       in map head grouped
        FullHouse -> let grouped = sortBy (comparing (Down . length)) $ group $ sort ranks
                     in map head grouped
        ThreeOfAKind -> let grouped = sortBy (comparing (Down . length)) $ group $ sort ranks
                        in map head grouped
        TwoPair -> let grouped = sortBy (comparing (Down . length)) $ group $ sort ranks
                       pairs = take 2 grouped
                       kicker = drop 2 grouped
                   in map head (sortBy (comparing (Down . head)) pairs) ++ map head kicker
        OnePair -> let grouped = sortBy (comparing (Down . length)) $ group $ sort ranks
                   in map head grouped
        Straight -> if ranks == [Two, Three, Four, Five, Ace]
                   then [Five] -- Low ace straight
                   else [maximum ranks]
        _ -> reverse $ sort ranks -- High card and flush
  in (handType, tieBreakers)

isStraightSequence :: [Rank] -> Bool
isStraightSequence ranks
  | ranks == [Two, Three, Four, Five, Ace] = True -- Low ace straight
  | length ranks == 5 = 
      let sortedRanks = sort ranks
          minRank = minimum sortedRanks
          maxRank = maximum sortedRanks
      in sortedRanks == [minRank .. maxRank]
  | otherwise = False
