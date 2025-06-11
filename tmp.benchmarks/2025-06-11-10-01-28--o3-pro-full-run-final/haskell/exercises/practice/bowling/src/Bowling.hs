module Bowling (score, BowlingError(..)) where

-- | All the problems that might be encountered while scoring a game.
--
--   IncompleteGame
--     The list of rolls ended before a complete, valid game was played.
--
--   InvalidRoll
--     A particular roll (identified by its 0-based index in the input list)
--     is impossible according to the rules of bowling – e.g. it is negative,
--     greater than 10, or makes the total number of pins for the frame
--     exceed 10 (except in the very last frame when fill-balls are allowed).
data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

-- | Score a full game of bowling represented as a list of rolls.
--
--   The function performs a complete validation of the input according to
--   the rules given in the exercise description and returns either the
--   cumulative score or an explanation of the first error encountered.
score :: [Int] -> Either BowlingError Int
score rolls =
  case parseFrames rolls of
    Left err      -> Left err
    Right frames  -> Right (computeScore frames)

---------------------------------------------------------------------------
-- Internal implementation
---------------------------------------------------------------------------

-- | Parse the list of rolls into the 10 logical frames of a bowling game.
--
--   The returned list ALWAYS contains exactly 10 frames when successful.
--
--   For frames 1-9 each frame is either
--     •  [10]          – strike
--     •  [x,y]         – spare (x + y == 10) or open (x + y < 10)
--
--   The 10th frame is
--     •  [10,a,b]      – strike followed by two fill balls
--     •  [x,10-x,b]    – spare followed by one fill ball (x < 10)
--     •  [x,y]         – open frame (x + y < 10 , x < 10)
--
--   The parser performs every bit of validation required by the rules.
parseFrames :: [Int] -> Either BowlingError [[Int]]
parseFrames = go 0 0
  where
    -- go frameIdx rollIdx remainingRolls = Either BowlingError framesSoFar
    go :: Int -> Int -> [Int] -> Either BowlingError [[Int]]
    -- Successfully collected 10 frames -> ensure no extra rolls
    go 10 _ []  = Right []
    go 10 rIdx (v:_) = Left (InvalidRoll rIdx v)
    -- Parse frames 1-9
    go fIdx rIdx rs
      | fIdx < 9 = parseNormalFrame rs
      | otherwise = parseTenthFrame rs
      where
        -- ---------------------------------------------------------------
        -- Frames 1-9
        parseNormalFrame :: [Int] -> Either BowlingError [[Int]]
        parseNormalFrame [] = Left IncompleteGame
        parseNormalFrame (r1:rest)
          | not (validRoll r1) = Left (InvalidRoll rIdx r1)
          | r1 == 10 = do
              next <- go (fIdx + 1) (rIdx + 1) rest
              pure ([10] : next)
          | otherwise =
              case rest of
                [] -> Left IncompleteGame
                (r2:rest')
                  | not (validRoll r2)       -> Left (InvalidRoll (rIdx + 1) r2)
                  | r1 + r2 > 10             -> Left (InvalidRoll (rIdx + 1) r2)
                  | otherwise                -> do
                      next <- go (fIdx + 1) (rIdx + 2) rest'
                      pure ([r1, r2] : next)

        -- ---------------------------------------------------------------
        -- Frame 10 (special rules)
        parseTenthFrame :: [Int] -> Either BowlingError [[Int]]
        parseTenthFrame rs10 = do
          (frame, consumed) <- parseTenth rs10
          let rest = drop consumed rs10
          if null rest
             then Right [frame]            -- Exactly ten frames, done.
             else case rest of
                    (v:_) -> Left (InvalidRoll (rIdx + consumed) v)
                    _     -> error "unreachable"

        -- Return (frame, rolls consumed)
        parseTenth :: [Int] -> Either BowlingError ([Int], Int)
        parseTenth (r1:rs)
          | not (validRoll r1) = Left (InvalidRoll rIdx r1)
          -- 10th frame begins with a strike
          | r1 == 10 = case rs of
              [] -> Left IncompleteGame
              (r2:rs') | not (validRoll r2) -> Left (InvalidRoll (rIdx + 1) r2)
                       | r2 == 10 -> case rs' of
                           [] -> Left IncompleteGame
                           (r3:_) | not (validRoll r3) -> Left (InvalidRoll (rIdx + 2) r3)
                                  | otherwise          -> Right ([10,10,r3], 3)
                       | otherwise -> case rs' of
                           [] -> Left IncompleteGame
                           (r3:_) | not (validRoll r3) -> Left (InvalidRoll (rIdx + 2) r3)
                                  | r2 + r3 > 10       -> Left (InvalidRoll (rIdx + 2) r3)
                                  | otherwise          -> Right ([10,r2,r3], 3)
          -- 10th frame does NOT begin with a strike
          | otherwise = case rs of
              [] -> Left IncompleteGame
              (r2:rs')
                | not (validRoll r2) -> Left (InvalidRoll (rIdx + 1) r2)
                | r1 + r2 > 10       -> Left (InvalidRoll (rIdx + 1) r2)
                -- Spare -> need one fill ball
                | r1 + r2 == 10 -> case rs' of
                    [] -> Left IncompleteGame
                    (r3:_) | not (validRoll r3) -> Left (InvalidRoll (rIdx + 2) r3)
                           | otherwise          -> Right ([r1,r2,r3], 3)
                -- Open frame -> done
                | otherwise -> Right ([r1,r2], 2)
        parseTenth [] = Left IncompleteGame

    -- Individual roll must be in [0,10]
    validRoll :: Int -> Bool
    validRoll x = x >= 0 && x <= 10

---------------------------------------------------------------------------
-- Scoring once the frames are known to be valid
---------------------------------------------------------------------------

computeScore :: [[Int]] -> Int
computeScore frames =
  let go :: Int -> [[Int]] -> Int
      go 9 (f:_) = sum f        -- Tenth frame
      go n (f:fs) =
        let restRolls = concat fs
            frameScore
              -- Strike
              | f == [10]           = 10 + sum (take 2 restRolls)
              -- Spare
              | sum f == 10         = 10 + head restRolls
              -- Open
              | otherwise           = sum f
        in frameScore + go (n + 1) fs
      go _ _ = 0                 -- Impossible due to prior validation
  in go 0 frames
