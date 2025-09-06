module Poker (bestHands) where

import Data.List (sortBy, groupBy, sort, nub, maximumBy)
import Data.Function (on)
import Data.Char (isDigit)

-- | Returns the best hand(s) from a list of poker hands.
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands = case validRankedHands of
    [] -> Nothing
    _ -> Just [hand | (hand, rank) <- validRankedHands, rank == snd bestRank]
  where
    rankedHands = map (\hand -> (hand, rateHand hand)) hands
    validRankedHands = [(h, r) | (h, Just r) <- rankedHands]
    bestRank = maximumBy (compare `on` snd) validRankedHands

-- | Rate a hand, returning a tuple that can be used for comparison
rateHand :: String -> Maybe (HandRank, [CardValue])
rateHand handStr = 
    case parseHand handStr of
        Nothing -> Nothing
        Just cards -> 
            let values = map fst cards
                suits = map snd cards
                isFlush = length (nub suits) == 1
                sortedValues = reverse $ sort values
                isStraight = isConsecutive sortedValues || (sortedValues == [14,5,4,3,2]) -- A-5 straight
                
                -- Special case for A-5 straight
                adjustedValues = if sortedValues == [14,5,4,3,2] then [5,4,3,2,1] else sortedValues
                
            in Just $ if isStraight && isFlush
               then (StraightFlush, adjustedValues)
               else if hasNOfKind 4 values
                    then (FourOfAKind, sortByCountThenValue values)
                    else if hasFullHouse values
                         then (FullHouse, sortByCountThenValue values)
                         else if isFlush
                              then (Flush, adjustedValues)
                              else if isStraight
                                   then (Straight, adjustedValues)
                                   else if hasNOfKind 3 values
                                        then (ThreeOfAKind, sortByCountThenValue values)
                                        else if hasTwoPair values
                                             then (TwoPair, sortByCountThenValue values)
                                             else if hasNOfKind 2 values
                                                  then (OnePair, sortByCountThenValue values)
                                                  else (HighCard, adjustedValues)

-- | Parse a hand string into a list of (value, suit) pairs
parseHand :: String -> Maybe [(CardValue, Char)]
parseHand str = 
    let cardStrs = words str
    in if length cardStrs /= 5
       then Nothing
       else case mapM parseCard cardStrs of
            Nothing -> Nothing
            Just cards -> Just cards
  where
    parseCard cs = 
        if null cs 
        then Nothing
        else let valueStr = take (length cs - 1) cs
                 suit = last cs
             in if null valueStr || not (isValidSuit suit)
                then Nothing
                else case parseValue valueStr of
                     Nothing -> Nothing
                     Just val -> Just (val, suit)
    
    isValidSuit s = s `elem` ['♠', '♣', '♥', '♦', 'S', 'C', 'H', 'D', 's', 'c', 'h', 'd']

    parseValue "A" = Just 14
    parseValue "K" = Just 13
    parseValue "Q" = Just 12
    parseValue "J" = Just 11
    parseValue "T" = Just 10
    parseValue [c] | isDigit c && c > '0' && c <= '9' = Just (read [c])
    parseValue _ = Nothing

-- | Check if values form a consecutive sequence
isConsecutive :: [Int] -> Bool
isConsecutive [] = True
isConsecutive [_] = True
isConsecutive (x:y:xs) = x == y + 1 && isConsecutive (y:xs)

-- | Check if hand has N of a kind
hasNOfKind :: Int -> [CardValue] -> Bool
hasNOfKind n values = any ((== n) . length) (groupValues values)

-- | Check if hand has a full house
hasFullHouse :: [CardValue] -> Bool
hasFullHouse values = 
    let groups = groupValues values
        groupSizes = sort $ map length groups
    in groupSizes == [2, 3]

-- | Check if hand has two pair
hasTwoPair :: [CardValue] -> Bool
hasTwoPair values = 
    let groups = groupValues values
    in length (filter ((== 2) . length) groups) == 2

-- | Group values by equality
groupValues :: [CardValue] -> [[CardValue]]
groupValues = groupBy (==) . sort

-- | Sort values by count (descending), then by value (descending)
sortByCountThenValue :: [CardValue] -> [CardValue]
sortByCountThenValue values = concat $ 
    sortBy (\a b -> if length a == length b 
                    then compare (maximum b) (maximum a)  -- Use maximum instead of head for safety
                    else compare (length b) (length a)) 
    (groupValues values)

-- | Card value type alias
type CardValue = Int

-- | Hand ranking data type
data HandRank = HighCard
              | OnePair
              | TwoPair
              | ThreeOfAKind
              | Straight
              | Flush
              | FullHouse
              | FourOfAKind
              | StraightFlush
              deriving (Eq, Ord, Show)
