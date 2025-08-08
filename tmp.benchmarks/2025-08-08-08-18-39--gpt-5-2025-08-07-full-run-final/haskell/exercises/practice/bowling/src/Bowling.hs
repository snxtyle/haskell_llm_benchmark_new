module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  let indexed = zip [0..] rolls
  (firstNine, rest) <- parseFirstNine indexed
  (tenthVals, leftover) <- parseTenth rest
  case leftover of
    [] -> do
      let totalFirstNine = sum (map (scoreFrame rolls) firstNine)
          totalTenth = sum tenthVals
      pure (totalFirstNine + totalTenth)
    ((ix, v):_) -> Left (InvalidRoll ix v)

-- Internal frame representation for frames 1..9
data Frame
  = FOpen   { fStart :: Int, fR1 :: Int, fR2 :: Int }
  | FSpare  { fStart :: Int, fR1 :: Int, fR2 :: Int }
  | FStrike { fStart :: Int }
  deriving (Eq, Show)

-- Parse the first nine frames, validating per-frame constraints
parseFirstNine :: [(Int, Int)] -> Either BowlingError ([Frame], [(Int, Int)])
parseFirstNine xs = go 9 xs []
  where
    go 0 rest acc = Right (reverse acc, rest)
    go n rest acc = do
      (f, rest') <- parseOneFrame rest
      go (n - 1) rest' (f : acc)

-- Parse a single frame (for frames 1..9)
parseOneFrame :: [(Int, Int)] -> Either BowlingError (Frame, [(Int, Int)])
parseOneFrame [] = Left IncompleteGame
parseOneFrame ((i1, r1) : xs) = do
  validateRange (i1, r1) 0 10
  if r1 == 10
    then Right (FStrike i1, xs)
    else case xs of
      [] -> Left IncompleteGame
      ((i2, r2) : rest) -> do
        validateRange (i2, r2) 0 10
        if r1 + r2 > 10
          then Left (InvalidRoll i2 r2)
          else
            if r1 + r2 == 10
              then Right (FSpare i1 r1 r2, rest)
              else Right (FOpen  i1 r1 r2, rest)

-- Parse the tenth frame and any required fill balls,
-- validating 10th-frame specific constraints.
parseTenth :: [(Int, Int)] -> Either BowlingError ([Int], [(Int, Int)])
parseTenth [] = Left IncompleteGame
parseTenth ((i1, r1) : rest) = do
  validateRange (i1, r1) 0 10
  if r1 == 10
    then case rest of
      [] -> Left IncompleteGame
      ((i2, r2) : rest2) -> do
        validateRange (i2, r2) 0 10
        case rest2 of
          [] -> Left IncompleteGame
          ((i3, r3) : rest3) -> do
            if r2 == 10
              then validateRange (i3, r3) 0 10
              else validateRange (i3, r3) 0 (10 - r2)
            Right ([r1, r2, r3], rest3)
    else case rest of
      [] -> Left IncompleteGame
      ((i2, r2) : rest2) -> do
        validateRange (i2, r2) 0 10
        if r1 + r2 > 10
          then Left (InvalidRoll i2 r2)
          else
            if r1 + r2 == 10
              then case rest2 of
                [] -> Left IncompleteGame
                ((i3, r3) : rest3) -> do
                  validateRange (i3, r3) 0 10
                  Right ([r1, r2, r3], rest3)
              else Right ([r1, r2], rest2)

-- Compute score for a single frame (1..9), using the full rolls list for bonuses
scoreFrame :: [Int] -> Frame -> Int
scoreFrame rs (FOpen _ a b)   = a + b
scoreFrame rs (FSpare i _ _)  = 10 + bonus1 rs (i + 2)
scoreFrame rs (FStrike i)     = 10 + bonus2 rs (i + 1)

bonus1 :: [Int] -> Int -> Int
bonus1 rs idx = case drop idx rs of
  (x:_) -> x
  _     -> 0 -- Unreachable if parsing ensured completeness

bonus2 :: [Int] -> Int -> Int
bonus2 rs idx = case drop idx rs of
  (a:b:_) -> a + b
  _       -> 0 -- Unreachable if parsing ensured completeness

-- Validate a roll value is within an inclusive range. On failure, return InvalidRoll at that index.
validateRange :: (Int, Int) -> Int -> Int -> Either BowlingError ()
validateRange (ix, v) lo hi =
  if v < lo || v > hi
    then Left (InvalidRoll ix v)
    else Right ()
