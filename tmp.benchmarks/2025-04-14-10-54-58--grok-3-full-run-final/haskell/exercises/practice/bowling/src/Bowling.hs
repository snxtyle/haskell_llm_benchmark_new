module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls
  | any invalidRoll rollsWithIndex = Left $ uncurry InvalidRoll (head $ filter invalidRoll rollsWithIndex)
  | not (isCompleteGame rolls) = Left IncompleteGame
  | otherwise = Right $ calculateScore rolls 0 0
  where
    rollsWithIndex = zip [0..] rolls
    invalidRoll (_, pins) = pins < 0 || pins > 10
    
    -- Check if the game is complete (handles 10th frame special cases)
    isCompleteGame rs
      | length rs < 10 = False
      | length (take 9 frames) < 9 = False
      | otherwise = case lastFrame of
          [x, y] -> x + y < 10  -- Open frame, no bonus rolls needed
          [x, y, z] -> x == 10 || (x + y == 10)  -- Strike or spare with bonus rolls
          _ -> False
      where
        frames = toFrames rs
        lastFrame = last frames
    
    -- Convert flat list of rolls to list of frames
    toFrames [] = []
    toFrames (10:rest) = [10] : toFrames rest
    toFrames (x:y:rest)
      | x + y == 10 = [x, y] : toFrames rest
      | otherwise = [x, y] : toFrames rest
    toFrames xs = [xs]  -- For the last frame with potential bonus rolls
    
    -- Calculate total score recursively
    calculateScore [] _ total = total
    calculateScore (r:rs) frameIndex total
      | frameIndex < 9 = case r of
          10 -> calculateScore rs (frameIndex + 1) (total + 10 + strikeBonus rs)
          _ -> case rs of
              (r2:rest) -> if r + r2 == 10
                           then calculateScore rest (frameIndex + 1) (total + 10 + spareBonus rest)
                           else calculateScore rest (frameIndex + 1) (total + r + r2)
              _ -> total  -- Shouldn't happen due to earlier validation
      | otherwise = total + sum (take 3 (r:rs))  -- 10th frame: sum all rolls (including bonus)
      where
        strikeBonus rolls' = sum $ take 2 rolls'
        spareBonus rolls' = head rolls'
