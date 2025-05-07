module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = go 1 rolls 0 0
  where
    valid r = r >= 0 && r <= 10
    go :: Int -> [Int] -> Int -> Int -> Either BowlingError Int
    go frame rs idx acc
      | frame <= 9 = case rs of
          [] -> Left IncompleteGame
          (r1:rest1)
            | not (valid r1) ->
                Left (InvalidRoll idx r1)
            | r1 == 10 ->  -- strike
                if length rest1 < 2
                  then Left IncompleteGame
                  else
                    let r2 = rest1 !! 0
                        r3 = rest1 !! 1
                    in case () of
                         _ | not (valid r2) -> Left (InvalidRoll (idx+1) r2)
                           | not (valid r3) -> Left (InvalidRoll (idx+2) r3)
                           | otherwise ->
                               let frameScore = 10 + r2 + r3
                               in go (frame+1) rest1 (idx+1) (acc + frameScore)
            | otherwise ->  -- r1 < 10
                case rest1 of
                  [] -> Left IncompleteGame
                  (r2:rest2)
                    | not (valid r2) ->
                        Left (InvalidRoll (idx+1) r2)
                    | r1 + r2 > 10 ->
                        Left (InvalidRoll (idx+1) r2)
                    | r1 + r2 == 10 ->  -- spare
                        if null rest2
                          then Left IncompleteGame
                          else
                            let r3 = head rest2
                            in if not (valid r3)
                                 then Left (InvalidRoll (idx+2) r3)
                                 else
                                   let frameScore = 10 + r3
                                   in go (frame+1) rest2 (idx+2) (acc + frameScore)
                    | otherwise ->  -- open frame
                        let frameScore = r1 + r2
                        in go (frame+1) rest2 (idx+2) (acc + frameScore)
      | frame == 10 = case rs of
          (r1:r2:rest)
            | not (valid r1) ->
                Left (InvalidRoll idx r1)
            | r1 == 10 ->  -- strike in 10th, expect two fill balls
                let firstBonus = r2 in
                if not (valid firstBonus)
                  then Left (InvalidRoll (idx+1) firstBonus)
                  else case rest of
                         [] -> Left IncompleteGame
                         (r3:extra)
                           | not (valid r3) ->
                               Left (InvalidRoll (idx+2) r3)
                           | not (null extra) ->
                               let extraRoll = head extra
                                   extraIdx  = idx + 3
                               in Left (InvalidRoll extraIdx extraRoll)
                           | otherwise ->
                               let frameScore = r1 + firstBonus + r3
                               in Right (acc + frameScore)
            | r1 + r2 == 10 ->  -- spare in 10th, expect one fill ball
                if null rest
                  then Left IncompleteGame
                  else
                    let r3   = head rest
                        extra = tail rest
                    in case () of
                         _ | not (valid r2)  -> Left (InvalidRoll (idx+1) r2)
                           | not (valid r3) -> Left (InvalidRoll (idx+2) r3)
                           | not (null extra) ->
                               let extraRoll = head extra
                                   extraIdx  = idx + 3
                               in Left (InvalidRoll extraIdx extraRoll)
                           | otherwise ->
                               let frameScore = r1 + r2 + r3
                               in Right (acc + frameScore)
            | r1 + r2 < 10 ->  -- open in 10th, exactly two rolls
                let extra = rest
                in case () of
                     _ | not (valid r2) ->
                           Left (InvalidRoll (idx+1) r2)
                       | not (null extra) ->
                           let extraRoll = head extra
                               extraIdx  = idx + 2
                           in Left (InvalidRoll extraIdx extraRoll)
                       | otherwise ->
                           Right (acc + r1 + r2)
            | otherwise ->  -- r1 + r2 > 10 in 10th open/spare validation
                Left (InvalidRoll (idx+1) r2)
          _ -> Left IncompleteGame
      | otherwise = Right acc
