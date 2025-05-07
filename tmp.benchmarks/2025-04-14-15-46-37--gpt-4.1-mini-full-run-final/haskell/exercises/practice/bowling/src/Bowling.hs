module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  validateRolls rolls
  calculateScore rolls

-- Validate all rolls for invalid pin counts and frame pin counts
validateRolls :: [Int] -> Either BowlingError ()
validateRolls rolls = go 0 rolls 1
  where
    go _ [] frame
      | frame <= 10 = Left IncompleteGame
      | otherwise = Right ()
    go idx (r:rs) frame
      | r < 0 || r > 10 = Left $ InvalidRoll idx r
      | frame > 10 = Right () -- extra rolls after 10th frame are fill balls, no validation needed here
      | otherwise =
          if frame < 10 then
            if r == 10 then -- strike frame, one roll only
              go (idx + 1) rs (frame + 1)
            else case rs of
              [] -> Left IncompleteGame
              (r2:rs2) ->
                if r + r2 > 10 then Left $ InvalidRoll (idx + 1) r2
                else go (idx + 2) rs2 (frame + 1)
          else -- 10th frame validation
            validateTenthFrame idx (r:rs)

    validateTenthFrame idx rolls10 =
      case rolls10 of
        (r1:r2:r3:_) ->
          if r1 == 10 then -- strike in first roll
            if r2 < 0 || r2 > 10 then Left $ InvalidRoll (idx + 1) r2
            else if r3 < 0 || r3 > 10 then Left $ InvalidRoll (idx + 2) r3
            else Right ()
          else if r1 + r2 == 10 then -- spare in first two rolls
            if r3 < 0 || r3 > 10 then Left $ InvalidRoll (idx + 2) r3
            else Right ()
          else if r1 + r2 < 10 then -- open frame, no third roll allowed
            Right ()
          else Left $ InvalidRoll (idx + 1) r2
        (r1:r2:[]) ->
          if r1 == 10 then Left IncompleteGame -- strike requires 2 fill balls
          else if r1 + r2 == 10 then Left IncompleteGame -- spare requires 1 fill ball
          else Right () -- open frame with 2 rolls
        _ -> Left IncompleteGame

-- Calculate the total score assuming rolls are valid
calculateScore :: [Int] -> Either BowlingError Int
calculateScore rolls = go 0 1 rolls
  where
    go total frame rs
      | frame > 10 = Right total
      | otherwise = case rs of
          [] -> Left IncompleteGame
          (r1:rest) ->
            if r1 == 10 then -- strike
              case rest of
                (r2:r3:_) ->
                  if r2 > 10 then Left $ InvalidRoll (length rolls - length rs + 1) r2
                  else if r3 > 10 then Left $ InvalidRoll (length rolls - length rs + 2) r3
                  else go (total + 10 + r2 + r3) (frame + 1) rest
                _ -> Left IncompleteGame
            else case rest of
              [] -> Left IncompleteGame
              (r2:rest2) ->
                let frameScore = r1 + r2 in
                if frameScore > 10 then Left $ InvalidRoll (length rolls - length rs + 1) r2
                else if frameScore == 10 then -- spare
                  case rest2 of
                    (r3:_) ->
                      if r3 > 10 then Left $ InvalidRoll (length rolls - length rs + 2) r3
                      else go (total + 10 + r3) (frame + 1) rest2
                    _ -> Left IncompleteGame
                else -- open frame
                  go (total + frameScore) (frame + 1) rest2
