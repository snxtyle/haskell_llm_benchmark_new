module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  (idxAfter9, total9) <- processFirst9 9 0
  (endIdx, tenthScore) <- scoreTenth idxAfter9
  if endIdx < len
    then Left (InvalidRoll endIdx (rolls !! endIdx))
    else Right (total9 + tenthScore)
  where
    len = length rolls

    -- Safe access with validation of pin count and presence.
    get :: Int -> Either BowlingError Int
    get i
      | i < 0      = Left IncompleteGame
      | i >= len   = Left IncompleteGame
      | otherwise  =
          let v = rolls !! i
          in if v < 0 || v > 10
               then Left (InvalidRoll i v)
               else Right v

    -- Process frames 1-9; returns (next roll index after 9 frames, accumulated score)
    processFirst9 :: Int -> Int -> Either BowlingError (Int, Int)
    processFirst9 0 idx = Right (idx, 0)
    processFirst9 n idx = do
      r1 <- get idx
      if r1 == 10
        then do
          b1 <- get (idx + 1)
          b2 <- get (idx + 2)
          (nextIdx, rest) <- processFirst9 (n - 1) (idx + 1)
          let frameScore = 10 + b1 + b2
          pure (nextIdx, frameScore + rest)
        else do
          r2 <- get (idx + 1)
          if r1 + r2 > 10
            then Left (InvalidRoll (idx + 1) r2)
            else do
              frameScore <- if r1 + r2 == 10
                              then do
                                b1 <- get (idx + 2)
                                pure (10 + b1)        -- spare
                              else pure (r1 + r2)     -- open
              (nextIdx, rest) <- processFirst9 (n - 1) (idx + 2)
              pure (nextIdx, frameScore + rest)

    -- Score the 10th frame starting at roll index idx.
    -- Returns (index after the 10th frame, 10th frame score).
    scoreTenth :: Int -> Either BowlingError (Int, Int)
    scoreTenth idx = do
      a <- get idx
      if a == 10
        then do
          b <- get (idx + 1)
          c <- get (idx + 2)
          -- In the 10th frame: if first fill isn't a strike, the two fills cannot sum > 10.
          if b /= 10 && b + c > 10
            then Left (InvalidRoll (idx + 2) c)
            else Right (idx + 3, 10 + b + c)
        else do
          b <- get (idx + 1)
          if a + b > 10
            then Left (InvalidRoll (idx + 1) b)
            else if a + b == 10
              then do
                c <- get (idx + 2)
                Right (idx + 3, 10 + c)
              else Right (idx + 2, a + b)
