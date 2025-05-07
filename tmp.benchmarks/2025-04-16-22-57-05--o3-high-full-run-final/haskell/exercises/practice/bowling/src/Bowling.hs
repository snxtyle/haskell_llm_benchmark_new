module Bowling (score, BowlingError(..)) where

-- | Possible problems while scoring a game.
data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

-- | Compute the final score for a sequence of rolls.
--
--   The list must represent a full (and valid) game of 10 frames.
--
--   If something is wrong, an appropriate 'BowlingError' is returned.
score :: [Int] -> Either BowlingError Int
score rolls = fmap fst (processFrames 0 1 rolls)
  where
    ------------------------------------------------------------------
    -- High‑level frame processing
    ------------------------------------------------------------------
    -- processFrames keeps track of the current roll index (for
    -- error reporting) and the current frame number (1‑10).
    --
    -- It returns the accumulated score and the roll index *after*
    -- the game finished.
    processFrames
        :: Int   -- ^ current roll index in the original list
        -> Int   -- ^ current frame (1..10)
        -> [Int] -- ^ remaining rolls
        -> Either BowlingError (Int, Int)
    processFrames _ _ [] = Left IncompleteGame
    processFrames idx frame rs
      | frame < 10 = do
          (frameScore, consumed) <- parseNormalFrame idx rs
          let idx' = idx + consumed
          let rs'  = drop consumed rs
          (rest, endIdx) <- processFrames idx' (frame + 1) rs'
          pure (frameScore + rest, endIdx)
      | frame == 10 = do
          (frameScore, consumed) <- parseTenthFrame idx rs
          let idx' = idx + consumed
          let remaining = drop consumed rs
          if null remaining
             then pure (frameScore, idx')
             else
               let extraVal = head remaining
               in Left (InvalidRoll idx' extraVal)
      | otherwise = Left IncompleteGame -- Should never happen

    ------------------------------------------------------------------
    -- Frame parsers
    ------------------------------------------------------------------
    -- Regular frames (1‑9)
    parseNormalFrame
        :: Int -> [Int] -> Either BowlingError (Int, Int)
    parseNormalFrame _ [] = Left IncompleteGame
    parseNormalFrame idx (r1:rs) = do
        validateRoll idx r1
        if r1 == 10
          -- strike
          then do
            r2 <- lookupRoll (idx + 1) rs
            r3 <- lookupRoll (idx + 2) (drop 1 rs)
            pure (10 + r2 + r3, 1)
          else do
            r2 <- secondRoll idx rs
            let base = r1 + r2
            if base > 10
               then Left (InvalidRoll (idx + 1) r2)
               else if base == 10
                       -- spare
                       then do
                         r3 <- lookupRoll (idx + 2) (drop 1 rs)
                         pure (10 + r3, 2)
                       else pure (base, 2)

    -- Tenth frame – its rules differ slightly from the others
    parseTenthFrame
        :: Int -> [Int] -> Either BowlingError (Int, Int)
    parseTenthFrame _ [] = Left IncompleteGame
    parseTenthFrame idx (r1:rs) = do
        validateRoll idx r1
        if r1 == 10
          -- strike in 10th frame
          then do
            r2 <- lookupRoll (idx + 1) rs
            r3 <- lookupRoll (idx + 2) (drop 1 rs)
            if r2 == 10
               then do
                 -- double strike: third roll can be anything 0‑10
                 validateRoll (idx + 2) r3
                 pure (10 + r2 + r3, 3)
               else do
                 -- second roll not strike: ensure r2 + r3 <= 10
                 validateRoll (idx + 1) r2
                 validateRoll (idx + 2) r3
                 if r2 + r3 > 10
                    then Left (InvalidRoll (idx + 2) r3)
                    else pure (10 + r2 + r3, 3)
          else do
            r2 <- secondRoll idx rs
            let base = r1 + r2
            if base > 10
               then Left (InvalidRoll (idx + 1) r2)
               else if base == 10
                       -- spare in 10th
                       then do
                         r3 <- lookupRoll (idx + 2) (drop 1 rs)
                         validateRoll (idx + 2) r3
                         pure (10 + r3, 3)
                       else pure (base, 2)

    ------------------------------------------------------------------
    -- Helpers
    ------------------------------------------------------------------
    -- Validate a single roll value (0‑10).
    validateRoll :: Int -> Int -> Either BowlingError ()
    validateRoll i v
      | v < 0 || v > 10 = Left (InvalidRoll i v)
      | otherwise       = Right ()

    -- Safely fetch the n‑th following roll, returning IncompleteGame
    -- if it doesn’t exist.
    lookupRoll :: Int -> [Int] -> Either BowlingError Int
    lookupRoll _ []     = Left IncompleteGame
    lookupRoll i (x:_)  = validateRoll i x >> pure x

    -- Fetch the second roll of a frame, validating that the frame
    -- doesn’t exceed 10 pins in that single roll. (The combined check
    -- with the first roll is performed by the caller.)
    secondRoll :: Int -> [Int] -> Either BowlingError Int
    secondRoll _ [] = Left IncompleteGame
    secondRoll idx (r2:_) = validateRoll (idx + 1) r2 >> pure r2
