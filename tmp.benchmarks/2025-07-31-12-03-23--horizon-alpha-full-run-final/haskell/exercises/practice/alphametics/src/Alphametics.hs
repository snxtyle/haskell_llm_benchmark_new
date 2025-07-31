module Alphametics (solve) where

import Data.Char (isUpper, isSpace)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- Public API
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
  let (addends, result) = parsePuzzle puzzle
      letters = uniqueLetters addends result
      leading = leadingLetters addends result
  in search addends result letters leading

-- Parse "SEND + MORE == MONEY" or "SEND + MORE = MONEY"
parsePuzzle :: String -> ([String], String)
parsePuzzle s =
  let cleaned = filter (\c -> isUpper c || c == '+' || c == '=' || isSpace c) s
      tokens = words cleaned
      -- tokens like ["SEND","+","MORE","=","MONEY"] or ["SEND","+","MORE","==","MONEY"]
      (lhs, rhs) =
        case break (\t -> t == "=" || t == "==") tokens of
          (ls, [_eq, r]) -> (filter (/= "+") ls, r)
          _              -> error "Invalid puzzle format"
  in (lhs, rhs)

uniqueLetters :: [String] -> String -> [Char]
uniqueLetters addends result =
  nub $ concat (result : addends)

leadingLetters :: [String] -> String -> S.Set Char
leadingLetters addends result =
  let heads = [head w | w <- result : addends, length w > 1]
  in S.fromList heads

-- Build columns from right to left for column-wise addition
-- Each column is a list of letters from addends and one result letter
-- e.g., for SEND + MORE = MONEY:
-- columns 0: [D,E] -> Y, 1: [N,R] -> E, 2: [E,O] -> N, 3: [S,M] -> O, 4: [] -> M
buildColumns :: [String] -> String -> [[(Maybe Char, [Char])]]
buildColumns addends result =
  -- we won't actually use this; kept for idea reference
  [[]]

-- Precompute column structure as ([addLetters], resultLetter) from rightmost to leftmost
makeColumnSpec :: [String] -> String -> [([Char], Char)]
makeColumnSpec addends result =
  let ra = map reverse addends
      rr = reverse result
      maxLen = maximum (map length (result:addends))
      col i =
        let adds = [w !! i | w <- ra, i < length w]
            res  = rr !! i
        in (adds, res)
  in [col i | i <- [0..maxLen-1]]

-- Backtracking search with carry propagation.
search :: [String] -> String -> [Char] -> S.Set Char -> Maybe [(Char, Int)]
search addends result letters leading =
  let cols = makeColumnSpec addends result
      -- heuristic: order letters by frequency (more constrained first), and ensure leading letters early
      freq = letterFrequency cols
      order = sortOn (negate . (`M.findWithDefault` 0) freq) letters
      initial = State
        { assign = M.empty
        , used = S.empty
        }
  in fmap M.toList (goCols cols 0 0 initial leading)

-- State for assignment
data State = State
  { assign :: M.Map Char Int
  , used   :: S.Set Int
  } deriving (Show)

letterFrequency :: [([Char], Char)] -> M.Map Char Int
letterFrequency cols =
  let allLetters = concat [res : adds | (adds, res) <- cols]
  in foldr (\c m -> M.insertWith (+) c 1 m) M.empty allLetters

-- Attempt to satisfy columns from right to left with carry
goCols :: [([Char], Char)] -> Int -> Int -> State -> S.Set Char -> Maybe (M.Map Char Int)
goCols cols idx carry st leading
  | idx == length cols = if carry == 0 then Just (assign st) else Nothing
  | otherwise =
      let (adds, res) = cols !! idx
      in assignColumn adds res carry st leading >>= \(sumVal, st') ->
           let digitRes = sumVal `mod` 10
               nextCarry = sumVal `div` 10
           in goCols cols (idx + 1) nextCarry st' leading

-- Assign digits to the letters of one column (addends and result), return total sum at this column
assignColumn :: [Char] -> Char -> Int -> State -> S.Set Char -> Maybe (Int, State)
assignColumn adds res carry st leading = do
  (sumAdds, st1) <- sumAddDigits adds st leading
  -- determine res digit to satisfy (sumAdds + carry) mod 10
  let want = (sumAdds + carry) `mod` 10
  st2 <- assignDigit res want st1 leading
  pure (sumAdds + carry, st2)

-- Sum digits of addend letters, assigning as needed
sumAddDigits :: [Char] -> State -> S.Set Char -> Maybe (Int, State)
sumAddDigits [] st _ = Just (0, st)
sumAddDigits (c:cs) st leading = do
  (d, st1) <- ensureAssigned c st leading
  (rest, st2) <- sumAddDigits cs st1 leading
  pure (d + rest, st2)

-- Ensure a letter has a digit; if not, try all allowed digits
ensureAssigned :: Char -> State -> S.Set Char -> Maybe (Int, State)
ensureAssigned c st leading =
  case M.lookup c (assign st) of
    Just d  -> Just (d, st)
    Nothing -> msumMap tryDigit candidates
      where
        noZero = c `S.member` leading
        digits = [0..9]
        candidates = filter (\d -> not (d `S.member` used st) && (not noZero || d /= 0)) digits
        tryDigit d =
          let st' = st { assign = M.insert c d (assign st)
                       , used   = S.insert d (used st)
                       }
          in Just (d, st')

-- Assign a specific digit to a letter, respecting constraints
assignDigit :: Char -> Int -> State -> S.Set Char -> Maybe State
assignDigit c d st leading =
  let noZero = c `S.member` leading
  in if noZero && d == 0 then Nothing
     else case M.lookup c (assign st) of
       Just d' -> if d' == d then Just st else Nothing
       Nothing ->
         if d `S.member` used st then Nothing
         else Just st { assign = M.insert c d (assign st)
                      , used   = S.insert d (used st)
                      }

-- Simple Maybe msum for trying alternatives
msumMap :: (a -> Maybe b) -> [a] -> Maybe b
msumMap _ [] = Nothing
msumMap f (x:xs) =
  case f x of
    Just y  -> Just y
    Nothing -> msumMap f xs
