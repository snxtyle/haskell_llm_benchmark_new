module TwoBucket (measure) where

-- | Represents which bucket to fill first
data Bucket = One | Two deriving (Eq, Show)

-- | State of the buckets: (moves, bucket1 amount, bucket2 amount)
type State = (Int, Int, Int)

-- | Solve the two bucket problem
-- Returns: (number of moves, (amount in target bucket, amount in other bucket))
measure :: Int -> Int -> Int -> Bucket -> Maybe (Int, (Int, Int))
measure size1 size2 target startBucket
  | target > max size1 size2 = Nothing
  | target `mod` gcd size1 size2 /= 0 = Nothing
  | otherwise = 
      let (moves, b1, b2) = solve size1 size2 target startBucket
      in case startBucket of
           One -> Just (moves, (b1, b2))
           Two -> Just (moves, (b2, b1))

-- | Solve the problem with given bucket sizes, target, and starting bucket
solve :: Int -> Int -> Int -> Bucket -> State
solve size1 size2 target startBucket = 
  case findSolution of
    Just state -> state
    Nothing -> error "No solution found"
  where
    -- Start with the appropriate bucket filled
    initialState = case startBucket of
      One -> (1, size1, 0)
      Two -> (1, 0, size2)
    
    -- Find solution using BFS
    findSolution = bfs [(initialState, [])] []
    
    -- BFS implementation
    bfs [] _ = Nothing
    bfs ((state@(_, b1, b2), visited):rest) explored
      | b1 == target = Just state
      | b2 == target = Just state
      | state `elem` explored = bfs rest explored
      | otherwise = 
          let nextStates = generateNextStates size1 size2 state
              validStates = filter (not . flip elem visited) nextStates
              newQueue = rest ++ [(s, state:visited) | s <- validStates]
          in bfs newQueue (state:explored)
    
    -- Generate all possible next states
    generateNextStates s1 s2 (moves, b1, b2) = 
      filter (not . violatesRule startBucket) $
        [ (moves + 1, s1, b2) | b1 < s1 ] ++  -- Fill bucket 1
        [ (moves + 1, b1, s2) | b2 < s2 ] ++  -- Fill bucket 2
        [ (moves + 1, 0, b2) | b1 > 0 ] ++    -- Empty bucket 1
        [ (moves + 1, b1, 0) | b2 > 0 ] ++    -- Empty bucket 2
        [ (moves + 1, max 0 (b1 - (s2 - b2)), min s2 (b2 + b1)) | b1 > 0, b2 < s2 ] ++  -- Pour 1 into 2
        [ (moves + 1, min s1 (b1 + b2), max 0 (b2 - (s1 - b1))) | b2 > 0, b1 < s1 ]     -- Pour 2 into 1
    
    -- Check if state violates the rule: starting bucket empty and other bucket full
    violatesRule One (_, 0, s2') = s2' == size2
    violatesRule Two (_, s1', 0) = s1' == size1
    violatesRule _ _ = False
