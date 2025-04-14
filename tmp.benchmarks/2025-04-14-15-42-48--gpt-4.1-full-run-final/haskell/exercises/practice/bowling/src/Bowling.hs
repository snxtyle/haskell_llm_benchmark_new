module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = score' rolls 0 0
  where
    score' :: [Int] -> Int -> Int -> Either BowlingError Int
    score' _ 10 _ = Right 0
    score' [] frame rollIdx
      | frame < 10 = Left IncompleteGame
      | otherwise = Right 0
    score' (r1:r2:r3:rs) 9 rollIdx
      -- 10th frame
      | not (validRoll r1) = Left $ InvalidRoll rollIdx r1
      | r1 == 10 = -- strike in 10th
          if validRoll r2 && validRoll r3 && (r2 == 10 || r2 + r3 <= 10)
            then if null rs
              then Right (r1 + r2 + r3)
              else Left $ InvalidRoll (rollIdx+3) (head rs)
            else if not (validRoll r2) then Left $ InvalidRoll (rollIdx+1) r2
            else Left $ InvalidRoll (rollIdx+2) r3
      | not (validRoll r2) = Left $ InvalidRoll (rollIdx+1) r2
      | r1 + r2 == 10 = -- spare in 10th
          if validRoll r3
            then if null rs
              then Right (r1 + r2 + r3)
              else Left $ InvalidRoll (rollIdx+3) (head rs)
            else Left $ InvalidRoll (rollIdx+2) r3
      | r1 + r2 < 10 =
          if null rs
            then Right (r1 + r2)
            else Left $ InvalidRoll (rollIdx+2) (head rs)
      | otherwise = Left $ InvalidRoll (rollIdx+1) r2
    score' (r1:r2:rs) frame rollIdx
      | frame == 9 = score' (r1:r2:rs ++ [0]) frame rollIdx -- pad for 10th frame
      | not (validRoll r1) = Left $ InvalidRoll rollIdx r1
      | r1 == 10 = -- strike
          case (rs) of
            (n1:n2:_) ->
              if validRoll n1 && validRoll n2
                then do
                  rest <- score' (r2:rs) (frame+1) (rollIdx+1)
                  Right (10 + n1 + n2 + rest)
                else if not (validRoll n1) then Left $ InvalidRoll (rollIdx+1) n1
                else Left $ InvalidRoll (rollIdx+2) n2
            _ -> Left IncompleteGame
      | not (validRoll r2) = Left $ InvalidRoll (rollIdx+1) r2
      | r1 + r2 > 10 = Left $ InvalidRoll (rollIdx+1) r2
      | r1 + r2 == 10 = -- spare
          case rs of
            (n:_) ->
              if validRoll n
                then do
                  rest <- score' rs (frame+1) (rollIdx+2)
                  Right (10 + n + rest)
                else Left $ InvalidRoll (rollIdx+2) n
            _ -> Left IncompleteGame
      | otherwise = do
          rest <- score' rs (frame+1) (rollIdx+2)
          Right (r1 + r2 + rest)
    score' (r1:rs) frame rollIdx
      | not (validRoll r1) = Left $ InvalidRoll rollIdx r1
      | r1 == 10 =
          case rs of
            (n1:n2:_) ->
              if validRoll n1 && validRoll n2
                then do
                  rest <- score' rs (frame+1) (rollIdx+1)
                  Right (10 + n1 + n2 + rest)
                else if not (validRoll n1) then Left $ InvalidRoll (rollIdx+1) n1
                else Left $ InvalidRoll (rollIdx+2) n2
            _ -> Left IncompleteGame
      | otherwise = Left IncompleteGame

    validRoll :: Int -> Bool
    validRoll n = n >= 0 && n <= 10
