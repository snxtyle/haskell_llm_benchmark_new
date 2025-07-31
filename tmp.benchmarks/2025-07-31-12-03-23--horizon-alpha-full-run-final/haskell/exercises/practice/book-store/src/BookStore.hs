module BookStore (total, Book(..)) where

import Data.List (sort, group)

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord)

-- Price in cents for a set size, after discount
priceForSet :: Int -> Int
priceForSet n =
  case n of
    1 -> 1 * 800
    2 -> 2 * 800 * 95 `div` 100   -- 5% discount
    3 -> 3 * 800 * 90 `div` 100   -- 10% discount
    4 -> 4 * 800 * 80 `div` 100   -- 20% discount
    5 -> 5 * 800 * 75 `div` 100   -- 25% discount
    _ -> error "Invalid set size"

-- Convert basket to counts per title
counts :: [Book] -> [Int]
counts bs =
  let grouped = map length . group . sort $ bs
      -- Ensure we have exactly 5 positions for the five titles
      fillTo5 xs = take 5 (xs ++ repeat 0)
  in fillTo5 grouped

-- Greedily build maximal distinct sets until all counts are zero.
-- Returns a multiset as list of set sizes.
greedySets :: [Int] -> [Int]
greedySets cs
  | all (== 0) cs = []
  | otherwise =
      let takeOneIfPositive x = if x > 0 then (1, x - 1) else (0, x)
          takenAndRest = map takeOneIfPositive cs
          taken = sum (map fst takenAndRest)
          rest  = map snd takenAndRest
      in taken : greedySets rest

-- Optimization: replace as many (5,3) pairs with (4,4) pairs as possible,
-- which is always cheaper: price(5)+price(3) > 2*price(4).
optimizeSets :: [Int] -> [Int]
optimizeSets sizes =
  let c5 = length (filter (== 5) sizes)
      c3 = length (filter (== 3) sizes)
      k  = min c5 c3
      others = filter (\x -> x /= 5 && x /= 3) sizes
      rem5 = c5 - k
      rem3 = c3 - k
  in others ++ replicate (2 * k) 4 ++ replicate rem5 5 ++ replicate rem3 3

total :: [Book] -> Int
total basket =
  let cs    = counts basket
      sets  = greedySets cs
      sets' = optimizeSets sets
  in sum (map priceForSet sets')
