module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  validatedRolls <- validateRolls rolls
  calculateScore validatedRolls

-- Validate all rolls
validateRolls :: [Int] -> Either BowlingError [Int]
validateRolls rolls = go rolls (0 :: Int) (0 :: Int)
  where
    go [] _ frame
      | frame == 10 = Right rolls
      | otherwise = Left IncompleteGame
    go (r:rs) idx frame
      | r < 0 || r > 10 = Left $ InvalidRoll idx r
      | frame < 9 = 
          if r == 10
            then go rs (idx + 1) (frame + 1)  -- Strike
            else case rs of
              [] -> Left IncompleteGame
              (r2:rs2) 
                | r2 < 0 || r2 > 10 -> Left $ InvalidRoll (idx + 1) r2
                | r + r2 > 10 -> Left $ InvalidRoll (idx + 1) r2
                | otherwise -> go rs2 (idx + 2) (frame + 1)
      | frame == 9 = validate10thFrame (r:rs) idx
      | otherwise = Left $ InvalidRoll idx r

-- Validate the 10th frame
validate10thFrame :: [Int] -> Int -> Either BowlingError [Int]
validate10thFrame rolls idx = case rolls of
  [r1, r2, r3] 
    | r1 == 10 -> 
        if r2 < 0 || r2 > 10 then Left $ InvalidRoll (idx + 1) r2
        else if r3 < 0 || r3 > 10 then Left $ InvalidRoll (idx + 2) r3
        else if r2 /= 10 && r2 + r3 > 10 then Left $ InvalidRoll (idx + 2) r3
        else Right rolls
    | r1 + r2 == 10 ->
        if r3 < 0 || r3 > 10 then Left $ InvalidRoll (idx + 2) r3
        else Right rolls
    | r1 + r2 < 10 -> Left $ InvalidRoll (idx + 2) r3
    | otherwise -> Left $ InvalidRoll (idx + 1) r2
  [r1, r2]
    | r1 == 10 -> Left IncompleteGame
    | r1 + r2 == 10 -> Left IncompleteGame
    | r1 + r2 < 10 -> Right rolls
    | otherwise -> Left $ InvalidRoll (idx + 1) r2
  [r1]
    | r1 == 10 -> Left IncompleteGame
    | otherwise -> Left IncompleteGame
  _ -> Left IncompleteGame

-- Calculate the score
calculateScore :: [Int] -> Either BowlingError Int
calculateScore rolls = 
  let result = scoreFrames rolls 0 0
  in case result of
    Left err -> Left err
    Right (total, _) -> Right total

-- Score all frames
scoreFrames :: [Int] -> Int -> Int -> Either BowlingError (Int, [Int])
scoreFrames rolls frame total
  | frame == 10 = Right (total, rolls)
  | frame == 9 = score10thFrame rolls total
  | otherwise = case rolls of
      [] -> Left IncompleteGame
      (10:rest) -> 
        case rest of
          (r1:r2:_) -> scoreFrames rest (frame + 1) (total + 10 + r1 + r2)
          _ -> Left IncompleteGame
      (r1:r2:rest)
        | r1 + r2 == 10 ->
            case rest of
              (r3:_) -> scoreFrames rest (frame + 1) (total + 10 + r3)
              _ -> Left IncompleteGame
        | otherwise -> scoreFrames rest (frame + 1) (total + r1 + r2)
      _ -> Left IncompleteGame

-- Score the 10th frame
score10thFrame :: [Int] -> Int -> Either BowlingError (Int, [Int])
score10thFrame rolls total = case rolls of
  [r1, r2, r3] 
    | r1 == 10 || r1 + r2 == 10 -> Right (total + r1 + r2 + r3, [])
    | r1 + r2 < 10 -> Right (total + r1 + r2, [r3])
    | otherwise -> Left IncompleteGame
  [r1, r2]
    | r1 == 10 || r1 + r2 == 10 -> Left IncompleteGame
    | r1 + r2 < 10 -> Right (total + r1 + r2, [])
    | otherwise -> Left IncompleteGame
  _ -> Left IncompleteGame
