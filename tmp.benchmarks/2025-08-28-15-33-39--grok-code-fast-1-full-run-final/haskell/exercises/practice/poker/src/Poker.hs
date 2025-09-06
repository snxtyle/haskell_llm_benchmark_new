module Poker (bestHands) where

import Data.List (group, sort, sortBy)
import Data.Maybe (isNothing, catMaybes)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Show, Enum, Bounded)

data Suit = Hearts | Diamonds | Spades | Clubs
  deriving (Eq, Show)

data Card = Card Rank Suit
  deriving (Eq, Show)

rankToInt :: Rank -> Int
rankToInt r = fromEnum r + 2

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
parseSuit 'S' = Just Spades
parseSuit 'C' = Just Clubs
parseSuit _ = Nothing

parseCard :: String -> Maybe Card
parseCard s = case s of
  [r, su] -> Card <$> parseRank r <*> parseSuit su
  '1':'0':su -> Card <$> Just Ten <*> parseSuit su
  _ -> Nothing

parseHand :: String -> Maybe [Card]
parseHand s = mapM parseCard (words s)

checkStraight :: [Int] -> Bool
checkStraight xs =
  let sorted = sort xs
  in consecutive sorted || sorted == [2, 3, 4, 5, 14]
  where
    consecutive [a, b, c, d, e] = b == a + 1 && c == b + 1 && d == c + 1 && e == d + 1
    consecutive _ = False

groupRanks :: [Rank] -> [(Rank, Int)]
groupRanks rs = map (\g -> (head g, length g)) (group (sort rs))

evaluateHand :: [Card] -> (Int, [Int])
evaluateHand hand =
  let ranks = map (\(Card r _) -> r) hand
      suits = map (\(Card _ s) -> s) hand
      rankInts = map rankToInt ranks
      sortedRanksDesc = sortBy (flip compare) ranks
      sortedIntsDesc = map rankToInt sortedRanksDesc
      sortedIntsAsc = sort rankInts
      isFlush = case suits of
        [] -> False
        (x:xs) -> all (== x) xs
      isStraight = checkStraight rankInts
      highStraight = if sortedIntsAsc == [2, 3, 4, 5, 14] then 5 else maximum sortedIntsAsc
      groups = sortBy (\(r1, l1) (r2, l2) -> compare (l2, r1) (l1, r2)) (groupRanks ranks)
      sizes = reverse (sort (map snd groups))
  in case sizes of
       [4, 1] ->
         let (quadRank, _) = head groups
             (kickerRank, _) = groups !! 1
         in (7, [rankToInt quadRank, rankToInt kickerRank])
       [3, 2] ->
         let (tripleRank, _) = head groups
             (pairRank, _) = groups !! 1
         in (6, [rankToInt tripleRank, rankToInt pairRank])
       [3, 1, 1] ->
         let (tripleRank, _) = head groups
             kickers = map fst (tail groups)
             kickerInts = map rankToInt kickers
         in (3, rankToInt tripleRank : kickerInts)
       [2, 2, 1] ->
         let (pair1, _) = head groups
             (pair2, _) = groups !! 1
             (kicker, _) = groups !! 2
             pairs = reverse (sort [rankToInt pair1, rankToInt pair2])
         in (2, pairs ++ [rankToInt kicker])
       [2, 1, 1, 1] ->
         let (pairRank, _) = head groups
             kickers = map fst (tail groups)
             kickerInts = reverse (sort (map rankToInt kickers))
         in (1, rankToInt pairRank : kickerInts)
       [1, 1, 1, 1, 1] ->
         if isStraight && isFlush then
           if sortedIntsAsc == [10, 11, 12, 13, 14] then (9, [14]) else (8, [highStraight])
         else if isFlush then (5, sortedIntsDesc)
              else if isStraight then (4, [highStraight])
                   else (0, sortedIntsDesc)
       _ -> error "Invalid hand"

bestHands :: [String] -> Maybe [String]
bestHands input =
  let parsed = map parseHand input
  in if any isNothing parsed
     then Nothing
     else let hands = catMaybes parsed
              evaluated = zip input (map evaluateHand hands)
              maxEval = maximum (map snd evaluated)
              best = filter (\(_, e) -> e == maxEval) evaluated
          in Just (map fst best)
