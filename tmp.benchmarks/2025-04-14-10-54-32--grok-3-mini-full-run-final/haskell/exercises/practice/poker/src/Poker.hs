module Poker (bestHands) where

import Data.List (sort, group, sortBy)
import Data.Char (toUpper, isAlpha, isDigit)
import Data.Ord (comparing)

-- Define types for cards and hands
type Rank = Int  -- 2-14, where 11=Jack, 12=Queen, 13=King, 14=Ace
type Suit = Char  -- 'H', 'D', 'C', 'S'
type Card = (Rank, Suit)
type Hand = [Card]

-- Parse a string into a Hand (list of Cards)
parseHand :: String -> Maybe Hand
parseHand s = mapM parseCard (words s)
  where
    parseCard :: String -> Maybe Card
    parseCard cs
      | length cs /= 2 = Nothing  -- Invalid card format
      | otherwise =
          let rankStr = head cs
              suit = toUpper (last cs)
          in case rankStr of
               '2' -> Just (2, suit)
               '3' -> Just (3, suit)
               '4' -> Just (4, suit)
               '5' -> Just (5, suit)
               '6' -> Just (6, suit)
               '7' -> Just (7, suit)
               '8' -> Just (8, suit)
               '9' -> Just (9, suit)
               'T' -> Just (10, suit)  -- Ten
               'J' -> Just (11, suit)  -- Jack
               'Q' -> Just (12, suit)  -- Queen
               'K' -> Just (13, suit)  -- King
               'A' -> Just (14, suit)  -- Ace
               _   -> Nothing  -- Invalid rank

-- Sort a hand by rank (descending for comparison)
sortHand :: Hand -> Hand
sortHand = sortBy (flip (comparing fst))  -- Sort by rank descending

-- Evaluate the rank of a hand
handRank :: Hand -> (Int, [Int])  -- (Category rank, tie-breaker values)
handRank hand =
    let sorted = sortHand hand
        ranks = map fst sorted
        suits = map snd sorted
        isFlush = all (== head suits) suits
        isStraight = let sortedRanks = map fst sorted
                         in sortedRanks == [a-4..a] || sortedRanks == [2,3,4,5,14]  -- Ace-low straight
        groups = sort $ map length $ group $ sort ranks  -- Grouped and sorted lengths
    in case (groups, isStraight, isFlush) of
         ([4,_], _)                  -> (8, ranks)  -- Four of a Kind
         ([3,2], _)                  -> (7, ranks)  -- Full House
         (_, True, True)             -> (9, ranks)  -- Straight Flush (overrides lower)
         (_, False, True)            -> (6, ranks)  -- Flush
         (_, True, False)            -> (5, ranks)  -- Straight
         ([3,1,1], _)                -> (4, ranks)  -- Three of a Kind
         ([2,2,1], _)                -> (3, ranks)  -- Two Pair
         ([2,1,1,1], _)              -> (2, ranks)  -- One Pair
         _                           -> (1, ranks)  -- High Card

-- Compare two hands based on their ranks
compareHands :: Hand -> Hand -> Ordering
compareHands h1 h2 =
    let (rank1, tieBreakers1) = handRank h1
        (rank2, tieBreakers2) = handRank h2
    in case compare rank1 rank2 of
         LT -> LT
         GT -> GT
         EQ -> comparing (reverse . take 5 . sort) tieBreakers1 tieBreakers2  -- Compare tie-breakers

-- Implement bestHands
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing  -- Empty list
bestHands handsList =
    case mapM parseHand handsList of  -- Try to parse all hands
        Nothing -> Nothing  -- If any hand is invalid, return Nothing
        Just parsedHands -> Just $ map unwords $  -- Return the best hands as strings
            map (map (\(r,s) -> rankToString r ++ [s]))  -- Convert back to string
            [ h | h <- bestParsedHands ]
    where
        sortedHands = map sortHand parsedHands  -- Sort each hand
        bestHand = maximumBy compareHands sortedHands  -- Get one best hand
        bestRank = handRank bestHand
        bestParsedHands = filter (\h -> handRank h == bestRank) sortedHands
        
        rankToString :: Rank -> String
        rankToString 11 = "J"
        rankToString 12 = "Q"
        rankToString 13 = "K"
        rankToString 14 = "A"
        rankToString n  = show n  -- 2-10
