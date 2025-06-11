module Alphametics (solve) where

import Data.Char (isUpper, isLetter)
import Data.List (nub, delete, sortOn)
import Data.Maybe (listToMaybe, fromMaybe)

-- | Solve an alphametics puzzle.
--   The puzzle must have the form
--
--     WORD [+ WORD ...] = RESULT
--   or
--     WORD [+ WORD ...] == RESULT
--
--   Examples of valid inputs:
--
--     "SEND + MORE = MONEY"
--     "I + BB == ILL"
--
--   The function returns the first solution it finds as a list of
--   (letter,digit) pairs.  If the puzzle is unsolvable it returns Nothing.
--
--   The search is a simple back-tracking enumeration with two small
--   optimisations:
--     * a leading letter can never be assigned the digit 0
--     * the solver stops as soon as it finds the first valid mapping
--
--   For the standard Exercism test-suite this is fast enough even for the
--   well-known “SEND + MORE = MONEY” puzzle which has 8 × 10⁶ candidate
--   assignments in the worst case.  A more sophisticated column-wise solver
--   would certainly be faster, but is not strictly necessary here and would
--   require substantially more code.
solve :: String -> Maybe [(Char, Int)]
solve rawPuzzle = do
  (addends, result) <- parsePuzzle rawPuzzle
  let letters        = nub $ concat (result : addends)
      leadingLetters = nub $ map head (filter ((>1) . length) (result : addends))
      flags          = [(c, c `elem` leadingLetters) | c <- letters]
      digits         = [0..9]
  fmap (sortOn fst) $ listToMaybe (search flags [] digits addends result)

-- ---------------------------------------------------------------------
-- Parsing
-- ---------------------------------------------------------------------

type WordStr = String -- a word consisting of upper-case letters

-- | Parse a puzzle string into (addends, result).
parsePuzzle :: String -> Maybe ([WordStr], WordStr)
parsePuzzle s =
  case splitAround (== '=') (clean s) of
    (lhs, rhs) | not (null rhs) && not (null lhsWords) ->
      Just (lhsWords, rhs)
      where lhsWords = splitWords lhs
    _ -> Nothing
  where
    -- Remove spaces and normalise "==" to "=" for simpler splitting.
    clean = filter (/= ' ') . replace "==" "="

-- Replace all occurrences of a sub-string with another sub-string.
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace needle repl haystack = go haystack
  where
    nlen = length needle
    go [] = []
    go xs
      | needle `prefixOf` xs = repl ++ go (drop nlen xs)
      | otherwise            = head xs : go (tail xs)
    prefixOf pfx xs = pfx == take (length pfx) xs

-- Split a list around the first element that satisfies a predicate.
splitAround :: (a -> Bool) -> [a] -> ([a], [a])
splitAround p xs = (takeWhile (not . p) xs, drop 1 (dropWhile (not . p) xs))

-- Split a plus-separated string into words.
splitWords :: String -> [WordStr]
splitWords str = filter (not . null) $ go str
  where
    go [] = [""]
    go (c:cs)
      | c == '+'  = "" : go cs
      | isLetter c && isUpper c = let (w:ws) = go cs in (c:w):ws
      | otherwise = go cs -- ignore anything unexpected

-- ---------------------------------------------------------------------
-- Back-tracking search
-- ---------------------------------------------------------------------

-- | Search for a valid mapping.
--
--   Arguments:
--     * list of (letter, isLeadingLetter) still to assign
--     * current partial mapping
--     * digits that are still unused
--     * list of addend words
--     * result word
search
  :: [(Char, Bool)]
  -> [(Char, Int)]
  -> [Int]
  -> [WordStr]
  -> WordStr
  -> [[(Char, Int)]]
search [] mapping _ addends result
  | evaluate addends mapping == evaluateWord result mapping = [mapping]
  | otherwise                                               = []
search ((c,isLead):rest) mapping digits addends result =
  concatMap tryDigit digits
  where
    tryDigit d
      | isLead && d == 0 = [] -- leading digit cannot be 0
      | otherwise        = search rest ((c,d):mapping) (delete d digits) addends result

-- ---------------------------------------------------------------------
-- Evaluation helpers
-- ---------------------------------------------------------------------

evaluate :: [WordStr] -> [(Char,Int)] -> Int
evaluate ws mp = sum (map (`evaluateWord` mp) ws)

evaluateWord :: WordStr -> [(Char,Int)] -> Int
evaluateWord w mp = foldl step 0 w
  where
    step acc ch = acc * 10 + lookupDigit ch mp

lookupDigit :: Char -> [(Char,Int)] -> Int
lookupDigit ch mp = fromMaybe (error "Incomplete mapping") (lookup ch mp)
