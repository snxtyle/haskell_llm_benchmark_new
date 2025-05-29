module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = 
  case validateGame 1 0 rolls of
    Left err -> Left err
    Right () -> Right $ sum rolls + computeBonus rolls
  where
    validateRoll idx r
      | r < 0 || r > 10 = Left (InvalidRoll idx r)
      | otherwise = Right ()

    validateGame frame _ [] 
      | frame > 10 = Right ()
      | otherwise = Left IncompleteGame
    validateGame frame idx (r:rs)
      | frame > 10 = Left (InvalidRoll idx r)
      | frame == 10 = 
          case rs of
            [] -> Left IncompleteGame
            (r2:rs2) -> 
              do
                validateRoll idx r
                validateRoll (idx+1) r2
                if r == 10 || r+r2 == 10
                  then 
                    case rs2 of
                      [] -> Left IncompleteGame
                      (r3:rs3) -> 
                        do
                          validateRoll (idx+2) r3
                          if r == 10 && r2 < 10 && r2 + r3 > 10
                            then Left (InvalidRoll (idx+2) r3)
                            else validateGame (frame+1) (idx+3) rs3
                  else 
                    if r+r2 > 10
                      then Left (InvalidRoll (idx+1) r2)
                      else validateGame (frame+1) (idx+2) rs2
      | otherwise = 
          do
            validateRoll idx r
            if r == 10
              then validateGame (frame+1) (idx+1) rs
              else 
                case rs of
                  [] -> Left IncompleteGame
                  (r2:rs2) -> 
                    do
                      validateRoll (idx+1) r2
                      if r+r2 > 10
                        then Left (InvalidRoll (idx+1) r2)
                        else validateGame (frame+1) (idx+2) rs2

    computeBonus gameRolls = 
      let loop frame idx acc
            | frame > 9 = acc
            | gameRolls!!idx == 10 =
                loop (frame+1) (idx+1) (acc + gameRolls!!(idx+1) + gameRolls!!(idx+2))
            | gameRolls!!idx + gameRolls!!(idx+1) == 10 =
                loop (frame+1) (idx+2) (acc + gameRolls!!(idx+2))
            | otherwise = 
                loop (frame+1) (idx+2) acc
      in loop 1 0 0
