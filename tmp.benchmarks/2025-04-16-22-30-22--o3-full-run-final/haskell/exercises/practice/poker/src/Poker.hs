module Poker (bestHands) where

import Data.Char (toUpper)
import Data.List (group, sort, sortBy)

-- | Determine the best poker hand(s) from a (possibly empty) list of hands.
--   Each hand is a String that contains exactly five cards separated by
--   single spaces. A card is represented by its rank followed by its suit,
--   e.g. "2H", "TD", "AS", or "10H".
--
--   The function returns `Nothing` for an empty input list, otherwise
--   `Just` the list of hand(s) that share the highest ranking (ties are
--   possible).  The order of the winning hands is the same as in the
--   original input.
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hands =
    let scored   = map (\h -> (h, scoreHand h)) hands
        best     = maximumByScore scored
        winners  = [h | (h, s) <- scored, s == best]
    in Just winners

----------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------

-- | A hand score consists of a primary category (larger is better)
--   and a list of tie‑break values.  The tuple derives Ord so it can
--   be compared directly.
type HandScore = (Int, [Int])

-- | Turn a hand String into its comparable score.
scoreHand :: String -> HandScore
scoreHand str =
    let cards      = map parseCard (words str)
        ranks      = map fst cards
        suits      = map snd cards
        sortedDesc = sortBy (flip compare) ranks
        rankGroups = sortBy (flip compare)
                     [ (length g, head g) | g <- group (sort ranks) ]
        counts     = map fst rankGroups
        kinds      = map snd rankGroups
        isFlush    = all (== head suits) suits
        (isStr, hiStraight) = straightInfo ranks
    in case () of
        _ | isFlush && isStr -> (9, [hiStraight])               -- Straight Flush
          | counts == [4,1]   -> (8, [kinds !! 0, kinds !! 1])  -- Four of a Kind
          | counts == [3,2]   -> (7, [kinds !! 0, kinds !! 1])  -- Full House
          | isFlush          -> (6, sortedDesc)                 -- Flush
          | isStr            -> (5, [hiStraight])               -- Straight
          | counts == [3,1,1]-> (4, [kinds !! 0] ++ sortDesc (drop 1 kinds))
          | counts == [2,2,1]-> (3, sortDesc [kinds !! 0, kinds !! 1] ++ [kinds !! 2])
          | counts == [2,1,1,1]
                              -> (2, [kinds !! 0] ++ sortDesc (drop 1 kinds))
          | otherwise        -> (1, sortedDesc)                 -- High Card
  where
    sortDesc = sortBy (flip compare)

-- | Parse a single card (e.g. "QS" or "10H") into (rank, suit).
--
--   The last character is always interpreted as the suit; everything before
--   it is treated as the rank.  Ranks can therefore be one character
--   ("2"‑"9", "T", "J", "Q", "K", "A") or two characters in the special
--   case of "10".
parseCard :: String -> (Int, Char)
parseCard card =
    case splitAt (length card - 1) card of
        (rankStr, [suitChar]) -> (rankValueStr rankStr, toUpper suitChar)
        _                     -> error "Invalid card"

-- | Convert a *string* representation of a rank into its numeric value.
rankValueStr :: String -> Int
rankValueStr r =
    case map toUpper r of
        "2"  -> 2
        "3"  -> 3
        "4"  -> 4
        "5"  -> 5
        "6"  -> 6
        "7"  -> 7
        "8"  -> 8
        "9"  -> 9
        "T"  -> 10
        "10" -> 10
        "J"  -> 11
        "Q"  -> 12
        "K"  -> 13
        "A"  -> 14
        _    -> error "Invalid rank"

-- | Determine if ranks make a straight and return its highest card.
straightInfo :: [Int] -> (Bool, Int)
straightInfo rs =
    let uniq = sort . unique $ rs
        consecutive xs = and $ zipWith (\a b -> b - a == 1) xs (tail xs)
    in case uniq of
        [2,3,4,5,14] -> (True, 5)                    -- Wheel straight
        xs | length xs == 5 && consecutive xs
           -> (True, last xs)
        _  -> (False, 0)
  where
    unique = map head . group . sort

-- | Find the maximum score among the list of (item,score) pairs.
maximumByScore :: Ord b => [(a,b)] -> b
maximumByScore = foldr1 max . map snd
