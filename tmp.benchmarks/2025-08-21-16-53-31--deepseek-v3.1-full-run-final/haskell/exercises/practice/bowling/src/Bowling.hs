module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = case validateRolls rolls of
  Left err -> Left err
  Right _ -> if isGameComplete rolls then Right (calculateScore rolls) else Left IncompleteGame

-- Check if the game is complete (has exactly 10 frames)
isGameComplete :: [Int] -> Bool
isGameComplete rolls = length (buildFrames rolls) == 10

-- Calculate the total score for a complete game
calculateScore :: [Int] -> Int
calculateScore rolls = sum (map (scoreFrame rolls) [0..9])
  where
    frames = buildFrames rolls
    
    scoreFrame :: [Int] -> Int -> Int
    scoreFrame rolls frameIndex
      | isStrike rolls frameIndex = 10 + strikeBonus rolls frameIndex
      | isSpare rolls frameIndex = 10 + spareBonus rolls frameIndex
      | otherwise = frameTotal rolls frameIndex

-- Build frames from rolls
buildFrames :: [Int] -> [[Int]]
buildFrames = buildFrames' 1
  where
    buildFrames' :: Int -> [Int] -> [[Int]]
    buildFrames' 11 _ = []  -- Game has only 10 frames
    buildFrames' frameNum [] = []
    buildFrames' frameNum (r:rs)
      | frameNum == 10 = [r:take 2 rs]  -- 10th frame can have up to 3 rolls
      | r == 10 = [r] : buildFrames' (frameNum + 1) rs  -- Strike
      | length rs >= 1 = [r, head rs] : buildFrames' (frameNum + 1) (tail rs)  -- Regular frame
      | otherwise = [[r]]

-- Check if a frame is a strike
isStrike :: [Int] -> Int -> Bool
isStrike rolls frameIndex = 
  let frame = buildFrames rolls !! frameIndex
  in not (null frame) && head frame == 10

-- Check if a frame is a spare
isSpare :: [Int] -> Int -> Bool
isSpare rolls frameIndex = 
  let frame = buildFrames rolls !! frameIndex
  in length frame >= 2 && sum (take 2 frame) == 10 && head frame /= 10

-- Calculate total for a frame
frameTotal :: [Int] -> Int -> Int
frameTotal rolls frameIndex = sum (buildFrames rolls !! frameIndex)

-- Calculate bonus for a strike
strikeBonus :: [Int] -> Int -> Int
strikeBonus rolls frameIndex = 
  let remainingRolls = drop (rollsConsumed frameIndex) rolls
  in sum (take 2 remainingRolls)

-- Calculate bonus for a spare
spareBonus :: [Int] -> Int -> Int
spareBonus rolls frameIndex = 
  let remainingRolls = drop (rollsConsumed frameIndex) rolls
  in head remainingRolls

-- Count how many rolls have been consumed up to a given frame
rollsConsumed :: Int -> Int
rollsConsumed frameIndex = sum (map frameRolls [0..frameIndex-1])
  where
    frameRolls i
      | i < 9 = if i == frameIndex then 0 else 1  -- For frames 1-9, we'll calculate properly
      | otherwise = 0

-- Better approach: count rolls consumed by walking through frames
rollsConsumed :: Int -> [Int] -> Int
rollsConsumed frameIndex rolls = 
  sum (map length (take frameIndex (buildFrames rolls)))

-- Let me rewrite the helper functions to use the buildFrames approach
strikeBonus :: [Int] -> Int -> Int
strikeBonus rolls frameIndex = 
  let frames = buildFrames rolls
      nextRolls = concat (drop (frameIndex + 1) frames)
  in sum (take 2 nextRolls)

spareBonus :: [Int] -> Int -> Int
spareBonus rolls frameIndex = 
  let frames = buildFrames rolls
      nextRolls = concat (drop (frameIndex + 1) frames)
  in head nextRolls

-- Validate all rolls
validateRolls :: [Int] -> Either BowlingError ()
validateRolls rolls = validateRolls' rolls 0 1
  where
    validateRolls' :: [Int] -> Int -> Int -> Either BowlingError ()
    validateRolls' [] _ _ = Right ()
    validateRolls' (r:rs) idx frameNum
      | r < 0 || r > 10 = Left (InvalidRoll idx r)
      | frameNum < 10 && r == 10 = validateRolls' rs (idx + 1) (frameNum + 1)  -- Strike
      | frameNum < 10 && not (null rs) = 
          if r + head rs > 10
            then Left (InvalidRoll idx r)
            else validateRolls' (tail rs) (idx + 2) (frameNum + 1)
      | frameNum == 10 = validateTenthFrame (r:rs) idx  -- Special validation for 10th frame
      | otherwise = Right ()  -- Incomplete frame, will be caught by isGameComplete
    
    validateTenthFrame :: [Int] -> Int -> Either BowlingError ()
    validateTenthFrame rolls idx
      | length rolls == 1 = Right ()
      | length rolls == 2 = if sum rolls > 20 then Left (InvalidRoll idx (head rolls)) else Right ()
      | length rolls >= 3 = if sum (take 2 rolls) > 20 || sum rolls > 30 
                            then Left (InvalidRoll idx (head rolls)) 
                            else Right ()
      | otherwise = Right ()
