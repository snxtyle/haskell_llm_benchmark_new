module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  validateRolls rolls 0
  if isCompleteGame rolls
    then Right (calculateScore rolls)
    else Left IncompleteGame

validateRolls :: [Int] -> Int -> Either BowlingError [Int]
validateRolls [] _ = Right []
validateRolls (r:rs) idx
  | r < 0 || r > 10 = Left (InvalidRoll idx r)
  | otherwise = do
      rest <- validateRolls rs (idx + 1)
      return (r : rest)

isCompleteGame :: [Int] -> Bool
isCompleteGame rolls = isCompleteGame' rolls 1

isCompleteGame' :: [Int] -> Int -> Bool
isCompleteGame' [] frame = frame > 10
isCompleteGame' rolls frame
  | frame > 10 = True
  | frame == 10 = isCompleteTenthFrame rolls
  | otherwise = 
      case rolls of
        (r1:rs) | r1 == 10 -> isCompleteGame' rs (frame + 1)  -- Strike
        (r1:r2:rs) -> 
          if r1 + r2 <= 10 
            then isCompleteGame' rs (frame + 1)  -- Normal frame or spare
            else False  -- Invalid frame
        _ -> False  -- Not enough rolls

isCompleteTenthFrame :: [Int] -> Bool
isCompleteTenthFrame (r1:r2:r3:_) = r1 == 10 || r1 + r2 == 10  -- Strike or spare needs 3 rolls
isCompleteTenthFrame (r1:r2:_) = r1 + r2 < 10  -- Open frame needs exactly 2 rolls
isCompleteTenthFrame _ = False

calculateScore :: [Int] -> Int
calculateScore rolls = calculateScore' rolls 1

calculateScore' :: [Int] -> Int -> Int
calculateScore' rolls frame
  | frame > 10 = 0
  | frame == 10 = scoreTenthFrame rolls
  | otherwise = 
      let (frameScore, remainingRolls) = scoreFrame rolls
      in frameScore + calculateScore' remainingRolls (frame + 1)

scoreFrame :: [Int] -> (Int, [Int])
scoreFrame (r1:rs) 
  | r1 == 10 = (10 + sum (take 2 rs), rs)  -- Strike
scoreFrame (r1:r2:rs)
  | r1 + r2 == 10 = (10 + head rs, rs)     -- Spare
  | otherwise = (r1 + r2, rs)              -- Open frame
scoreFrame _ = (0, [])

scoreTenthFrame :: [Int] -> Int
scoreTenthFrame rolls = 
  let tenthFrameRolls = drop (rollsBeforeFrame10 rolls) rolls
  in sum tenthFrameRolls

rollsBeforeFrame10 :: [Int] -> Int
rollsBeforeFrame10 rolls = rollsBeforeFrame10' rolls 1

rollsBeforeFrame10' :: [Int] -> Int -> Int
rollsBeforeFrame10' [] _ = 0
rollsBeforeFrame10' rolls frame
  | frame >= 10 = 0
  | otherwise = 
      case rolls of
        (r1:rs) | r1 == 10 -> 1 + rollsBeforeFrame10' rs (frame + 1)  -- Strike
        (r1:r2:rs) -> 2 + rollsBeforeFrame10' rs (frame + 1)           -- Two rolls
        _ -> 0
