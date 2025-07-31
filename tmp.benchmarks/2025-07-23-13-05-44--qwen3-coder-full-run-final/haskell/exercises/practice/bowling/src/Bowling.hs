module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  validatedRolls <- validateRolls rolls
  calculateScore validatedRolls

validateRolls :: [Int] -> Either BowlingError [Int]
validateRolls rolls = go 0 rolls
  where
    go _ [] = Right []
    go index (r:rs)
      | r < 0 = Left $ InvalidRoll { rollIndex = index, rollValue = r }
      | r > 10 = Left $ InvalidRoll { rollIndex = index, rollValue = r }
      | otherwise = (r :) <$> go (index + 1) rs

calculateScore :: [Int] -> Either BowlingError Int
calculateScore rolls = go 0 1 rolls
  where
    go acc 11 [] = Right acc  -- Game completed exactly
    go acc 11 _  = Left IncompleteGame  -- Extra rolls after game completion
    go acc frame [] = Left IncompleteGame  -- Not enough rolls
    go acc frame (r1:rs)
      | frame < 10 = 
          if r1 == 10  -- Strike
            then case rs of
              (r2:r3:rest) -> go (acc + 10 + r2 + r3) (frame + 1) rs
              _ -> Left IncompleteGame
            else case rs of  -- Not a strike
              (r2:rest) -> 
                if r1 + r2 < 10  -- Open frame
                  then go (acc + r1 + r2) (frame + 1) rest
                  else if r1 + r2 == 10  -- Spare
                    then case rest of
                      (r3:_) -> go (acc + 10 + r3) (frame + 1) rest
                      _ -> Left IncompleteGame
                    else Left $ InvalidRoll { rollIndex = 2*(frame-1)+1, rollValue = r2 }
              _ -> Left IncompleteGame
      | frame == 10 =  -- 10th frame
          if r1 == 10  -- Strike in 10th
            then case rs of
              (r2:r3:rest) -> 
                if null rest
                  then Right (acc + 10 + r2 + r3)
                  else Left IncompleteGame
              _ -> Left IncompleteGame
            else case rs of
              (r2:rest) -> 
                if r1 + r2 < 10  -- Open frame
                  then 
                    if null rest
                      then Right (acc + r1 + r2)
                      else Left IncompleteGame
                  else if r1 + r2 == 10  -- Spare
                    then case rest of
                      (r3:remaining) -> 
                        if null remaining
                          then Right (acc + 10 + r3)
                          else Left IncompleteGame
                      _ -> Left IncompleteGame
                    else Left $ InvalidRoll { rollIndex = 2*(frame-1)+1, rollValue = r2 }
              _ -> Left IncompleteGame
