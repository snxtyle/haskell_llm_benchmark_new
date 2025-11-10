module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = scoreHelper 0 0 rolls
  where
    -- Safely gets a roll at a specific index from the original list of rolls
    -- and validates its value. Returns an error if the roll is out of bounds
    -- or has an invalid value.
    getAndValidateRoll :: [Int] -> Int -> Either BowlingError Int
    getAndValidateRoll allRolls index
      | index >= length allRolls = Left IncompleteGame
      | rollValue < 0 || rollValue > 10 = Left (InvalidRoll index rollValue)
      | otherwise = Right rollValue
      where rollValue = allRolls !! index

    -- Recursive helper to score the game frame by frame.
    -- frameIndex: 0-9 (for 10 frames)
    -- currentScore: The accumulated score so far.
    -- remainingRolls: The list of rolls yet to be processed.
    scoreHelper :: Int -> Int -> [Int] -> Either BowlingError Int
    scoreHelper frameIndex currentScore remainingRolls
      -- Base case: All 10 frames have been scored.
      | frameIndex == 10 =
          if null remainingRolls
            then Right currentScore
            else Left (InvalidRoll (length rolls - length remainingRolls) (head remainingRolls))

      | otherwise = do
          let rollIndex = length rolls - length remainingRolls
          r1 <- getAndValidateRoll rolls rollIndex

          if frameIndex == 9
            then do -- 10th Frame logic
              r2 <- getAndValidateRoll rolls (rollIndex + 1)
              -- Check for invalid frame (sum of first two rolls > 10, unless first is a strike)
              if r1 + r2 > 10 && r1 < 10
                then Left (InvalidRoll (rollIndex + 1) r2)
                else do
                  (frameScore, rollsConsumed) <- case (r1 == 10, r1 + r2 == 10) of
                    (True, _) -> do -- Strike
                      r3 <- getAndValidateRoll rolls (rollIndex + 2)
                      return (r1 + r2 + r3, 3)
                    (_, True) -> do -- Spare
                      r3 <- getAndValidateRoll rolls (rollIndex + 2)
                      return (r1 + r2 + r3, 3)
                    _ -> do -- Open frame
                      return (r1 + r2, 2)

                  -- Check for extra rolls after the 10th frame is complete
                  if length rolls == rollIndex + rollsConsumed
                    then Right (currentScore + frameScore)
                    else Left (InvalidRoll (rollIndex + rollsConsumed) (rolls !! (rollIndex + rollsConsumed)))

            else -- Frames 1-9 logic
              if r1 == 10
                then do -- Strike
                  r2 <- getAndValidateRoll rolls (rollIndex + 1)
                  r3 <- getAndValidateRoll rolls (rollIndex + 2)
                  let frameScore = 10 + r2 + r3
                  scoreHelper (frameIndex + 1) (currentScore + frameScore) (tail remainingRolls)

                else do -- Not a strike, need a second roll
                  r2 <- getAndValidateRoll rolls (rollIndex + 1)
                  -- Check for invalid frame (sum of two rolls > 10)
                  if r1 + r2 > 10
                    then Left (InvalidRoll (rollIndex + 1) r2)
                    else
                      if r1 + r2 == 10
                        then do -- Spare
                          r3 <- getAndValidateRoll rolls (rollIndex + 2)
                          let frameScore = 10 + r3
                          scoreHelper (frameIndex + 1) (currentScore + frameScore) (drop 2 remainingRolls)
                        else do -- Open frame
                          let frameScore = r1 + r2
                          scoreHelper (frameIndex + 1) (currentScore + frameScore) (drop 2 remainingRolls)
