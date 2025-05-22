module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  -- First validate all rolls (individual and frame constraints)
  validateRolls rolls
  -- Then calculate the score
  case calculateScore 1 0 rolls of
    Just finalScore -> Right finalScore
    Nothing -> Left IncompleteGame

-- Validate rolls including frame constraints
validateRolls :: [Int] -> Either BowlingError ()
validateRolls = validateFrame 1 0
  where
    validateFrame :: Int -> Int -> [Int] -> Either BowlingError ()
    validateFrame frameNum idx rolls
      | frameNum > 10 = Right ()
      | frameNum == 10 = validateTenthFrame idx rolls
      | otherwise = case rolls of
          -- Strike
          (r:rest) | r < 0 || r > 10 -> Left $ InvalidRoll idx r
                   | r == 10 -> validateFrame (frameNum + 1) (idx + 1) rest
                   | otherwise -> case rest of
              -- Second roll of frame
              (r2:rest2) | r2 < 0 || r2 > 10 -> Left $ InvalidRoll (idx + 1) r2
                         | r + r2 > 10 -> Left $ InvalidRoll (idx + 1) r2
                         | otherwise -> validateFrame (frameNum + 1) (idx + 2) rest2
              -- Incomplete frame (will be caught later)
              [] -> Right ()
          -- No more rolls
          [] -> Right ()
    
    validateTenthFrame :: Int -> [Int] -> Either BowlingError ()
    validateTenthFrame idx rolls = case rolls of
      -- Strike in 10th frame
      (r1:rest) | r1 < 0 || r1 > 10 -> Left $ InvalidRoll idx r1
                | r1 == 10 -> case rest of
          (r2:rest2) | r2 < 0 || r2 > 10 -> Left $ InvalidRoll (idx + 1) r2
                     | otherwise -> case rest2 of
              (r3:_) | r3 < 0 || r3 > 10 -> Left $ InvalidRoll (idx + 2) r3
                     | r2 == 10 -> Right ()  -- Two strikes, r3 can be anything 0-10
                     | r2 + r3 > 10 -> Left $ InvalidRoll (idx + 2) r3
                     | otherwise -> Right ()
              [] -> Right ()  -- Will be caught as incomplete
          [] -> Right ()  -- Will be caught as incomplete
                | otherwise -> case rest of
          -- Not a strike, check for spare or open frame
          (r2:rest2) | r2 < 0 || r2 > 10 -> Left $ InvalidRoll (idx + 1) r2
                     | r1 + r2 > 10 -> Left $ InvalidRoll (idx + 1) r2
                     | r1 + r2 == 10 -> case rest2 of  -- Spare
              (r3:_) | r3 < 0 || r3 > 10 -> Left $ InvalidRoll (idx + 2) r3
                     | otherwise -> Right ()
              [] -> Right ()  -- Will be caught as incomplete
                     | otherwise -> Right ()  -- Open frame, no bonus roll
          [] -> Right ()  -- Will be caught as incomplete
      [] -> Right ()  -- Will be caught as incomplete

-- Calculate the score frame by frame
calculateScore :: Int -> Int -> [Int] -> Maybe Int
calculateScore frameNum acc rolls
  | frameNum > 10 = Just acc
  | frameNum == 10 = scoreTenthFrame acc rolls
  | otherwise = case rolls of
      -- Strike: all 10 pins knocked down in first roll
      (10:rest) -> 
        case rest of
          (b:c:_) -> calculateScore (frameNum + 1) (acc + 10 + b + c) rest
          _ -> Nothing  -- Not enough rolls after strike
      
      -- Two rolls in the frame
      (a:b:rest)
        | a + b == 10 -> -- Spare
            case rest of
              (c:_) -> calculateScore (frameNum + 1) (acc + 10 + c) rest
              _ -> Nothing  -- Not enough rolls after spare
        | otherwise -> -- Open frame
            calculateScore (frameNum + 1) (acc + a + b) rest
      
      -- Not enough rolls to complete frame
      _ -> Nothing

-- Special handling for the 10th frame
scoreTenthFrame :: Int -> [Int] -> Maybe Int
scoreTenthFrame acc rolls = case rolls of
  -- Strike in 10th frame - need 2 more rolls
  (10:b:c:_) -> Just (acc + 10 + b + c)
  
  -- Two rolls that make a spare - need 1 more roll
  (a:b:c:_) | a + b == 10 && a /= 10 -> Just (acc + 10 + c)
  
  -- Open frame in 10th - just two rolls
  (a:b:[]) | a + b < 10 -> Just (acc + a + b)
  
  -- Any other case is incomplete
  _ -> Nothing
