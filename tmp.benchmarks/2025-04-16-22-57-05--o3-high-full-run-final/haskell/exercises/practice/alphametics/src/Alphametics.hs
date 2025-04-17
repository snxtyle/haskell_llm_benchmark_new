module Alphametics (solve) where

import Data.List (nub, delete, sortOn, foldl')

-- | Solve an alphametics puzzle.
--   The input must have the form
--
--        "WORD [+ WORD ...] == WORD"
--
--   Each different letter is replaced by a different digit.
--   Leading letters of multi–digit words may not be mapped to 0.
--   If the puzzle is solvable the first solution found is returned.
--   The resulting association list is sorted alphabetically by the letter.
--
--   Examples:
--
--     >>> solve "SEND + MORE == MONEY"
--     Just [('D',7),('E',5),('M',1),('N',6),('O',0),('R',8),('S',9),('Y',2)]
--
--     >>> solve "THIS + AIN'T + NO + PUZZLE == IT"
--     Nothing
--
solve :: String -> Maybe [(Char, Int)]
solve puzzle =
  do
    (operands, result) <- parsePuzzle puzzle
    let letters = nub $ concat (result : operands)
    -- More than ten distinct symbols – impossible to solve.
    if length letters > 10
       then Nothing
       else do
         let leadingLetters = [ head w | w <- result : operands, length w > 1 ]
         solution <- search letters leadingLetters operands result [] [0..9]
         -- sort alphabetically as expected by the tests
         return $ sortOn fst solution

type Mapping = [(Char, Int)]

----------------------------------------------------------------------
-- Parsing
----------------------------------------------------------------------

-- | Very small, whitespace‑based parser.
--
--   Returns the list of operand words (left side of “==”)
--   and the single result word (right side of “==”).
--
--   Any deviation from the expected syntax yields Nothing.
parsePuzzle :: String -> Maybe ([String], String)
parsePuzzle str =
  let tokens          = words str
      (lhs, rhsEq)    = break (=="==") tokens
  in case rhsEq of
       ("==":rhs) ->
         case rhs of
           [res] ->
             let operands = filter (/= "+") lhs
             in if null operands then Nothing else Just (operands, res)
           _      -> Nothing
       _ -> Nothing

----------------------------------------------------------------------
-- Back‑tracking search
----------------------------------------------------------------------

-- | Recursively assign digits to the given letters.
--
--   The function stops as soon as a valid complete assignment is found.
search
  :: [Char]      -- ^ letters that still need a digit
  -> [Char]      -- ^ letters that may not be mapped to 0
  -> [String]    -- ^ left‑hand operand words
  -> String      -- ^ result word
  -> Mapping     -- ^ partial assignment constructed so far
  -> [Int]       -- ^ digits that are still free
  -> Maybe Mapping
search [] _ operands result mapping _ =
  if valid mapping operands result
     then Just mapping
     else Nothing
search (c:cs) leading operands result mapping freeDigits =
  attempt freeDigits
 where
  attempt []     = Nothing
  attempt (d:ds)
    -- leading letter may not be zero
    | c `elem` leading && d == 0 = attempt ds
    | otherwise =
        case search cs leading operands result ((c,d):mapping)
                                         (delete d freeDigits) of
          Just sol -> Just sol
          Nothing  -> attempt ds

----------------------------------------------------------------------
-- Verifying a complete assignment
----------------------------------------------------------------------

-- | Check whether a completed mapping satisfies the equation.
valid :: Mapping -> [String] -> String -> Bool
valid mapping operands result =
  let valueOf = wordValue mapping
      leftSum = sum (map valueOf operands)
      rightV  = valueOf result
  in leftSum == rightV

-- | Translate a word to its numeric value using the given assignment.
wordValue :: Mapping -> String -> Int
wordValue mapping =
  foldl' (\acc ch -> acc * 10 + digit ch) 0
 where
  digit ch = case lookup ch mapping of
               Just v  -> v
               Nothing -> error "Incomplete mapping (should never happen)"
