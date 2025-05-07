module Bowling (score, BowlingError(..)) where

-- | Possible problems that can occur while scoring a game.
data BowlingError
    = IncompleteGame                    -- ^ Not enough rolls to finish the game
    | InvalidRoll { rollIndex :: Int    -- ^ Index (0‑based) of the offending roll
                  , rollValue :: Int    -- ^ The illegal pin count that was given
                  }
    deriving (Eq, Show)

-- | Compute the total score of a bowling game.
--
-- The function validates the list of rolls, returning the earliest
-- encountered problem (using 0‑based indices), or the final score
-- if everything is correct.
score :: [Int] -> Either BowlingError Int
score rolls = go 0 1 0
  where
    totalRolls = length rolls

    -- Safe accessor that also performs the individual‑roll validation
    getRoll :: Int -> Either BowlingError Int
    getRoll idx
      | idx >= totalRolls        = Left IncompleteGame
      | v < 0 || v > 10          = Left (InvalidRoll idx v)
      | otherwise                = Right v
      where
        v = rolls !! idx

    -- Recursive worker:
    --   * current roll index
    --   * current frame (1‑based, 1 .. 10)
    --   * running score
    go :: Int -> Int -> Int -> Either BowlingError Int
    -- -----------------------------------------------------------------
    -- Frames 1 .. 9
    go idx frame acc
      | frame <= 9 = do
          r1 <- getRoll idx
          if r1 == 10
            -- Strike
            then do
              b1 <- getRoll (idx + 1)
              b2 <- getRoll (idx + 2)
              go (idx + 1) (frame + 1) (acc + 10 + b1 + b2)
            else do
              r2 <- getRoll (idx + 1)
              -- validate that the two rolls do not knock down more than 10 pins
              if r1 + r2 > 10
                then Left (InvalidRoll (idx + 1) r2)
                else
                  let frameScore = if r1 + r2 == 10
                                     then doSpareBonus (idx + 2)
                                     else pure (r1 + r2)
                  in frameScore >>= \fs -> go (idx + 2) (frame + 1) (acc + fs)
      where
        -- spare bonus is the very next roll
        doSpareBonus bIdx = do
          bonus <- getRoll bIdx
          pure (10 + bonus)

    -- -----------------------------------------------------------------
    -- Frame 10 (final frame)
    go idx 10 acc = do
      r1 <- getRoll idx
      if r1 == 10
        -- Strike in 10th frame → two fill balls
        then do
          r2 <- getRoll (idx + 1)
          r3 <- getRoll (idx + 2)
          -- If the second roll isn't a strike, the third must respect remaining pins
          if r2 /= 10 && r2 + r3 > 10
            then Left (InvalidRoll (idx + 2) r3)
            else finalize (idx + 3) (acc + 10 + r2 + r3)
        else do
          r2 <- getRoll (idx + 1)
          -- open frame invalid if pins exceed 10
          if r1 + r2 > 10
            then Left (InvalidRoll (idx + 1) r2)
            else
              if r1 + r2 == 10
                -- Spare → one fill ball
                then do
                  r3 <- getRoll (idx + 2)
                  finalize (idx + 3) (acc + 10 + r3)
                -- Open frame → no fill balls
                else finalize (idx + 2) (acc + r1 + r2)

    -- -----------------------------------------------------------------
    -- Any frame number greater than 10 should never occur
    go _ _ _ = Left IncompleteGame

    -- After finishing the 10th frame, ensure there are no superfluous rolls
    finalize :: Int -> Int -> Either BowlingError Int
    finalize nextIdx total
      | nextIdx < totalRolls = Left (InvalidRoll nextIdx (rolls !! nextIdx))
      | otherwise            = Right total
