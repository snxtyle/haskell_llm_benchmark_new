module Poker (bestHands) where

import Data.List (sortBy, groupBy, nub, sort)
import Data.Ord (comparing, Down(..))
import Data.Maybe (catMaybes, isJust)
import Control.Arrow ((&&&))
import Data.Function (on)

-- Data types

-- Suit
data Suit = Spade | Heart | Diamond | Club
  deriving (Eq, Ord, Enum, Bounded, Show)

-- Rank (ordered from Two to Ace)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded, Show)

-- Card
data Card = Card { rank :: Rank, suit :: Suit }
  deriving (Eq, Show)

-- Order cards by rank for sorting purposes if needed, though explicit sort by rank is often used.
instance Ord Card where
  compare c1 c2 = compare (rank c1) (rank c2)

-- HandRankValue: Represents the poker hand ranking.
-- Constructors are ordered from lowest to highest rank.
-- Fields within constructors are ordered for tie-breaking (e.g., higher pair first).
data HandRankValue
  = HighCard [Rank]             -- Ranks of 5 cards, sorted descending
  | OnePair Rank [Rank]         -- Pair Rank, 3 kickers sorted descending
  | TwoPair Rank Rank Rank      -- High Pair Rank, Low Pair Rank, Kicker Rank
  | ThreeOfAKind Rank [Rank]    -- Three-of-a-kind Rank, 2 kickers sorted descending
  | Straight Rank               -- Highest rank in straight (Ace can be high in T-A or low in A-5 where 5 is high)
  | Flush [Rank]                -- Ranks of 5 cards, sorted descending
  | FullHouse Rank Rank         -- Three-of-a-kind Rank, Pair Rank
  | FourOfAKind Rank Rank       -- Four-of-a-kind Rank, Kicker Rank
  | StraightFlush Rank          -- Highest rank in straight flush
  deriving (Eq, Ord, Show)

-- ParsedHand: A tuple of the list of cards and its original string representation.
type ParsedHand = ([Card], String)

-- EvaluatedHand: Stores the original string, parsed cards, and the determined HandRankValue.
data EvaluatedHand = EvaluatedHand
    { originalString :: String
    , cards :: [Card]
    , handRankValue :: HandRankValue
    } deriving (Show)

-- Compare EvaluatedHand based on handRankValue for finding the maximum.
instance Eq EvaluatedHand where
  eh1 == eh2 = handRankValue eh1 == handRankValue eh2

instance Ord EvaluatedHand where
  compare eh1 eh2 = compare (handRankValue eh1) (handRankValue eh2)


-- Parsing functions

parseCard :: String -> Maybe Card
parseCard s =
  let (rankStr, suitStr) = if length s == 3 && take 2 s == "10" -- Handle "10S", "10H", etc.
                           then (take 2 s, drop 2 s)
                           else if length s == 2
                                then (take 1 s, drop 1 s)
                                else ("", "") -- Invalid length
      mRank = case rankStr of
        "2" -> Just Two
        "3" -> Just Three
        "4" -> Just Four
        "5" -> Just Five
        "6" -> Just Six
        "7" -> Just Seven
        "8" -> Just Eight
        "9" -> Just Nine
        "T" -> Just Ten
        "J" -> Just Jack
        "Q" -> Just Queen
        "K" -> Just King
        "A" -> Just Ace
        "10"-> Just Ten
        _   -> Nothing
      mSuit = case suitStr of
        "S" -> Just Spade
        "H" -> Just Heart
        "D" -> Just Diamond
        "C" -> Just Club
        _   -> Nothing
  in Card <$> mRank <*> mSuit

parseHand :: String -> Maybe ParsedHand
parseHand s =
  let cardStrings = words s
  in if length cardStrings /= 5
     then Nothing
     else do
       parsedCards <- mapM parseCard cardStrings
       if length (nub parsedCards) /= 5 -- Check for duplicate cards
          then Nothing
          else Just (sortBy (comparing (Down . rank)) parsedCards, s) -- Store cards sorted by rank descending

-- Evaluation functions

evaluateHand :: ParsedHand -> EvaluatedHand
evaluateHand (cs, str) = EvaluatedHand str cs (calculateHandRank cs)

-- Helper: sort ranks descending
sortRanksDesc :: [Rank] -> [Rank]
sortRanksDesc = sortBy (comparing Down)

-- Helper: get ranks from cards
getRanks :: [Card] -> [Rank]
getRanks = map rank

-- Helper: group cards by rank, then count. Result is [(Rank, Count)]
rankCounts :: [Card] -> [(Rank, Int)]
rankCounts cs = map (rank . head &&& length) $ groupBy ((==) `on` rank) $ sortBy (comparing rank) cs

calculateHandRank :: [Card] -> HandRankValue
calculateHandRank handCards =
  -- handCards are already sorted by rank descending from parseHand
  let ranks = getRanks handCards -- These are sorted descending
      suits = map suit handCards
      isFlush = length (nub suits) == 1

      -- Check for straight:
      -- uniqueSortedRanksAsc: e.g., [Two,Three,Four,Five,Ace] for A-5 straight
      -- or [Nine,Ten,Jack,Queen,King] for 9-K straight.
      -- ranksInStraightOrderForCompare: Ranks used for comparison, e.g. for A-5 straight, it's [Five,Four,Three,Two,Ace]
      (isStraight, ranksInStraightOrderForCompare) =
        let uniqueSortedRanksAsc = sort $ nub ranks
        in if length uniqueSortedRanksAsc /= 5 then (False, []) -- Must have 5 distinct ranks for a straight
           else -- Ace-low straight (A,2,3,4,5), represented as [Five, Four, Three, Two, Ace] for comparison (Five is high)
                if uniqueSortedRanksAsc == [Two, Three, Four, Five, Ace]
                then (True, [Five, Four, Three, Two, Ace])
                -- General case for straights
                else if all (\(r1, r2) -> fromEnum r1 + 1 == fromEnum r2) (zip uniqueSortedRanksAsc (tail uniqueSortedRanksAsc))
                     then (True, reverse uniqueSortedRanksAsc) -- Store in descending order e.g. [Ace, King, Queen, Jack, Ten]
                     else (False, [])

      -- Highest card determining the straight's rank (e.g., Ace for T-A, Five for A-5)
      straightHighRank = if isStraight then head ranksInStraightOrderForCompare else Ace -- Dummy value if not straight

      -- rankGroups: List of (Rank, count) sorted by count descending, then by rank descending.
      -- e.g., for Full House KKKQQ: [(King,3), (Queen,2)]
      -- e.g., for Two Pair KKQQJ: [(King,2), (Queen,2), (Jack,1)]
      rankGroups = sortBy (comparing (Down . snd) <> comparing (Down . fst)) $ rankCounts handCards

  in case (isStraight, isFlush, rankGroups) of
    (True, True, _)                       -> StraightFlush straightHighRank
    (_,    _,    (rVal, 4):(kVal,1):_)    -> FourOfAKind rVal kVal
    (_,    _,    (r1Val,3):(r2Val,2):_)   -> FullHouse r1Val r2Val
    (_,    True, _)                       -> Flush ranks -- `ranks` are already sorted descending from handCards
    (True, _,    _)                       -> Straight straightHighRank
    (_,    _,    (rVal,3):(k1Val,1):(k2Val,1):_) -> ThreeOfAKind rVal (sortRanksDesc [k1Val, k2Val])
    (_,    _,    (r1Val,2):(r2Val,2):(kVal,1):_) -> TwoPair r1Val r2Val kVal -- r1Val > r2Val due to rankGroups sort
    (_,    _,    (rVal,2):(k1Val,1):(k2Val,1):(k3Val,1):_) -> OnePair rVal (sortRanksDesc [k1Val, k2Val, k3Val])
    _                                     -> HighCard ranks -- `ranks` are already sorted descending

-- Main function
bestHands :: [String] -> Maybe [String]
bestHands handStrings =
  let maybeParsedHands = map parseHand handStrings
  in if not (all isJust maybeParsedHands)
     then Nothing -- If any hand string is invalid, parsing fails for the entire set
     else
       let parsedHands = catMaybes maybeParsedHands -- All elements are Just, so this is safe
           evaluatedHands = map evaluateHand parsedHands
       in if null evaluatedHands
          then Just [] -- Input list was empty, or contained only unparseable hands (latter caught by all isJust)
          else
            -- Find the maximum rank value among all evaluated hands
            let maxRankValue = handRankValue $ maximum evaluatedHands
                -- Filter to get all hands that have this maximum rank value
                bestEvaluatedHands = filter (\eh -> handRankValue eh == maxRankValue) evaluatedHands
            -- Return the original string representations of these best hands
            in Just $ map originalString bestEvaluatedHands
