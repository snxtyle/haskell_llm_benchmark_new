module TwoBucket (measure) where

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (bucketOneSize, bucketTwoSize) target
  | target < 0 || target > max bucketOneSize bucketTwoSize = Nothing  -- Impossible cases
  | otherwise = bfsSearch bucketOneSize bucketTwoSize target (bucketOneSize, 0) 1 []  -- Start with bucket one full, moves = 1, empty visited list

bfsSearch :: Int -> Int -> Int -> (Int, Int) -> Int -> [(Int, Int)] -> Maybe (Int, (Int, Int))
bfsSearch bucketOneSize bucketTwoSize target start moves visited
  | fst start == target = Just (moves, (1, start))  -- Target in bucket one
  | snd start == target = Just (moves, (2, start))  -- Target in bucket two
  | otherwise = go bucketOneSize bucketTwoSize target [start] (moves + 1) (start : visited)  -- Continue BFS
  where
    go :: Int -> Int -> Int -> [(Int, Int)] -> Int -> [(Int, Int)] -> Maybe (Int, (Int, Int))
    go _ _ _ [] _ _ = Nothing  -- Queue empty, impossible
    go bucketOneSize bucketTwoSize target (current:queue) currentMoves visitedList =
      let neighborsList = filter (`notElem` visitedList) $ filter (not . invalid bucketTwoSize) $ neighbors bucketOneSize bucketTwoSize current
          newQueue = queue ++ neighborsList
          updatedVisited = visitedList ++ neighborsList  -- Add new states to visited
      in case go bucketOneSize bucketTwoSize target newQueue currentMoves updatedVisited of  -- Recurse with updated queue
           Nothing -> go bucketOneSize bucketTwoSize target neighborsList currentMoves updatedVisited  -- Check the new neighbors
           result -> result  -- Return if found

-- Generate possible new states from current state
neighbors :: Int -> Int -> (Int, Int) -> [(Int, Int)]
neighbors bucketOneSize bucketTwoSize (a, b) =
  [ pourFromFirstToSecond bucketTwoSize (a, b)
  , pourFromSecondToFirst bucketOneSize (a, b)
  , emptyFirst (a, b)
  , emptySecond (a, b)
  , fillFirst bucketOneSize (a, b)
  , fillSecond bucketTwoSize (a, b)
  ]

-- Helper functions for actions
pourFromFirstToSecond :: Int -> (Int, Int) -> (Int, Int)
pourFromFirstToSecond bucketTwoSize (a, b) =
  let spaceInSecond = bucketTwoSize - b
      amountToPour = min a spaceInSecond
  in (a - amountToPour, b + amountToPour)

pourFromSecondToFirst :: Int -> (Int, Int) -> (Int, Int)
pourFromSecondToFirst bucketOneSize (a, b) =
  let spaceInFirst = bucketOneSize - a
      amountToPour = min b spaceInFirst
  in (a + amountToPour, b - amountToPour)

emptyFirst :: (Int, Int) -> (Int, Int)
emptyFirst (a, b) = (0, b)

emptySecond :: (Int, Int) -> (Int, Int)
emptySecond (a, b) = (a, 0)

fillFirst :: Int -> (Int, Int) -> (Int, Int)
fillFirst bucketOneSize (a, b) = (bucketOneSize, b)

fillSecond :: Int -> (Int, Int) -> (Int, Int)
fillSecond bucketTwoSize (a, b) = (a, bucketTwoSize)

-- Check if a state is invalid: bucket one empty and bucket two full
invalid :: Int -> (Int, Int) -> Bool
invalid bucketTwoSize (x, y) = x == 0 && y == bucketTwoSize
