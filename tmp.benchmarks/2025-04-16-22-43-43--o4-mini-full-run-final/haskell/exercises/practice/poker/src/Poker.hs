module Poker (bestHands) where

import Data.List (sort, sortOn, group)
import Data.Ord (Down(Down))
import Data.Char (toUpper)
import Data.Maybe (mapMaybe)

type Card = (Int, Char)

-- Map card rank characters to numeric values
charValue :: Char -> Maybe Int
charValue c = case toUpper c of
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  'T' -> Just 10
  'J' -> Just 11
  'Q' -> Just 12
  'K' -> Just 13
  'A' -> Just 14
  _   -> Nothing

-- Parse a two-character card, e.g. "TS" or "7H"
parseCard :: String -> Maybe Card
parseCard [v,s] = do
  val <- charValue v
  return (val, s)
parseCard _ = Nothing

-- Parse a hand (5 cards separated by spaces)
parseHand :: String -> Maybe [Card]
parseHand = mapM parseCard . words

-- Compute a sortable score: (category, tiebreakers...)
-- Categories 0=HighCard … 8=StraightFlush
scoreHand :: [Card] -> (Int, [Int])
scoreHand cards =
  let values        = map fst cards
      valuesDesc    = sortOn Down values
      suits         = map snd cards
      isFlush       = all (== head suits) suits
      uniqAsc       = map head $ group $ sort values
      isWheel       = uniqAsc == [2,3,4,5,14]
      isStraightNorm= length uniqAsc == 5 && maximum uniqAsc - minimum uniqAsc == 4
      isStraight    = isStraightNorm || isWheel
      highStr       = if isWheel then 5 else maximum uniqAsc
      groups        = sortOn (\(cnt,val) -> (Down cnt, Down val))
                           [(length g, head g) | g <- group $ sort values]
      counts        = map fst groups
      groupValues   = map snd groups
  in case () of
       _ | isStraight && isFlush -> (8, [highStr])
         | counts == [4,1]       -> (7, groupValues)
         | counts == [3,2]       -> (6, groupValues)
         | isFlush               -> (5, valuesDesc)
         | isStraight            -> (4, [highStr])
         | counts == [3,1,1]     -> (3, groupValues)
         | counts == [2,2,1]     -> (2, groupValues)
         | counts == [2,1,1,1]   -> (1, groupValues)
         | otherwise             -> (0, valuesDesc)

-- Select the best hand(s); Nothing if input empty or no hands parse
bestHands :: [String] -> Maybe [String]
bestHands [] = Nothing
bestHands hs =
  let -- keep only successfully parsed hands
      zipped = mapMaybe (\h -> (h,) <$> parseHand h) hs
  in if null zipped
        then Nothing
        else
          let scored    = [ (h, scoreHand cards) | (h, cards) <- zipped ]
              bestScore = maximum $ map snd scored
          in Just [ h | (h, sc) <- scored, sc == bestScore ]
