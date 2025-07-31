module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

-- Public API: compute the score of a game given the sequence of rolls
score :: [Int] -> Either BowlingError Int
score rolls = do
  -- First, validate per-roll values are in [0..10]
  mapM_ (uncurry validateSingle) (zip [0..] rolls)
  -- Then, walk frames 1..9 validating frame sums and collecting score/position
  (posAfter9, total9) <- playFirstNine 0 0
  -- Finally, validate and score the 10th frame and any fill balls
  tenthScore <- scoreTenth posAfter9
  -- Ensure there are no extra rolls beyond what a complete game requires
  if posAfter9 + lengthOfTenth posAfter9 == length rolls
    then Right (total9 + tenthScore)
    else Left $ InvalidRoll { rollIndex = posAfter9 + lengthOfTenth posAfter9
                            , rollValue = rolls !! (posAfter9 + lengthOfTenth posAfter9)
                            }
  where
    n = length rolls

    rollAt i =
      if i < n then Right (rolls !! i)
      else Left IncompleteGame

    validateSingle :: Int -> Int -> Either BowlingError ()
    validateSingle idx v =
      if v < 0 || v > 10
        then Left (InvalidRoll idx v)
        else Right ()

    -- Process frames 1..9
    playFirstNine :: Int -> Int -> Either BowlingError (Int, Int)
    playFirstNine pos acc
      | frame == 10 = Right (pos, acc)
      | otherwise = do
          r1 <- rollAt pos
          if r1 == 10
            then do
              b1 <- rollAt (pos + 1)
              b2 <- rollAt (pos + 2)
              advance (pos + 1) (acc + 10 + b1 + b2)
            else do
              r2 <- rollAt (pos + 1)
              -- validate frame sum <= 10 for non-strike
              if r1 + r2 > 10
                then Left (InvalidRoll (pos + 1) r2)
                else
                  if r1 + r2 == 10
                    then do
                      b <- rollAt (pos + 2)
                      advance (pos + 2) (acc + 10 + b)
                    else
                      advance (pos + 2) (acc + r1 + r2)
      where
        frame = 1 + framesConsumed pos
        advance p a = either Left Right (playFirstNine p a)

    -- Count frames consumed so far (only for frames 1..9 here)
    framesConsumed :: Int -> Int
    framesConsumed pos = go 0 0
      where
        go i f
          | f == 9 || i >= n = f
          | otherwise =
              let r1 = rolls !! i
              in if r1 == 10 then go (i + 1) (f + 1)
                             else if i + 1 < n then go (i + 2) (f + 1) else f

    -- Score and validate tenth frame (starting at position pos)
    scoreTenth :: Int -> Either BowlingError Int
    scoreTenth pos = do
      r1 <- rollAt pos
      if r1 == 10
        then do
          f1 <- rollAt (pos + 1)
          f2 <- rollAt (pos + 2)
          -- For strike in 10th: if first fill is not strike, the two fills must not exceed 10
          if f1 == 10 || f1 + f2 <= 10
            then Right (10 + f1 + f2)
            else Left (InvalidRoll (pos + 2) f2)
        else do
          r2 <- rollAt (pos + 1)
          if r1 + r2 > 10
            then Left (InvalidRoll (pos + 1) r2)
            else if r1 + r2 == 10
              then do
                f1 <- rollAt (pos + 2)
                Right (10 + f1)
              else
                Right (r1 + r2)

    -- Determine how many rolls the 10th frame consumes starting at pos
    lengthOfTenth :: Int -> Int
    lengthOfTenth pos
      | pos >= n = 0
      | otherwise =
          let r1 = rolls !! pos
          in if r1 == 10
               then if pos + 2 < n then 3 else 0
               else if pos + 1 < n
                      then let r2 = rolls !! (pos + 1)
                           in if r1 + r2 == 10
                                then if pos + 2 < n then 3 else 0
                                else 2
                      else 0
