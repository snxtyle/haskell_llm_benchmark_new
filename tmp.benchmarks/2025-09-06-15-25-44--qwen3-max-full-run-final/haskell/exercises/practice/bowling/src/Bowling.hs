module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = calculateScore 0 1 rolls
  where
    calculateScore :: Int -> Int -> [Int] -> Either BowlingError Int
    calculateScore total 11 [] = Right total
    calculateScore _ 11 _ = Left IncompleteGame
    calculateScore _ _ [] = Left IncompleteGame
    calculateScore total frame (first:rest)
      | first < 0 || first > 10 = Left $ InvalidRoll (20 - length rest - 1) first
      | frame == 10 = scoreTenthFrame total first rest
      | first == 10 = -- Strike
          case rest of
            (next1:next2:_) -> 
              if next1 < 0 || next1 > 10 || next2 < 0 || (next1 /= 10 && next1 + next2 > 10) then
                Left $ InvalidRoll (21 - length rest) (if next1 < 0 || next1 > 10 then next1 else next2)
              else
                calculateScore (total + 10 + next1 + next2) (frame + 1) rest
            [next1] -> 
              if next1 < 0 || next1 > 10 then
                Left $ InvalidRoll (20 - length rest) next1
              else
                Left IncompleteGame
            [] -> Left IncompleteGame
      | otherwise = -- Open frame or spare
          case rest of
            [] -> Left IncompleteGame
            (second:remaining) ->
              if second < 0 || second > 10 || first + second > 10 then
                Left $ InvalidRoll (20 - length remaining) (if second < 0 || second > 10 then second else first + second)
              else if first + second == 10 then -- Spare
                case remaining of
                  [] -> Left IncompleteGame
                  (next:_) -> 
                    if next < 0 || next > 10 then
                      Left $ InvalidRoll (20 - length remaining) next
                    else
                      calculateScore (total + 10 + next) (frame + 1) remaining
              else -- Open frame
                calculateScore (total + first + second) (frame + 1) remaining

    scoreTenthFrame :: Int -> Int -> [Int] -> Either BowlingError Int
    scoreTenthFrame total first []
      | first < 10 = Left IncompleteGame
      | otherwise = Right (total + first)  -- Strike with no fill balls
    scoreTenthFrame total first (second:rest)
      | first < 10 = -- Open frame or spare in 10th
          if first + second > 10 || second < 0 || second > 10 then
            Left $ InvalidRoll (19 - length rest) (if second < 0 || second > 10 then second else first + second)
          else if first + second < 10 then -- Open frame
            if null rest then
              Right (total + first + second)
            else
              Left $ InvalidRoll (19 - length rest) (head rest)  -- Extra roll after open frame
          else -- Spare
            case rest of
              [] -> Left IncompleteGame
              [third] -> 
                if third < 0 || third > 10 then
                  Left $ InvalidRoll 20 third
                else
                  Right (total + first + second + third)
              _ -> Left $ InvalidRoll 20 (rest !! 1)  -- Too many rolls
      | otherwise = -- Strike in 10th
          if second < 0 || second > 10 then
            Left $ InvalidRoll 19 second
          else
            case rest of
              [] -> Left IncompleteGame  -- Need at least one more roll after strike
              [third] -> 
                if third < 0 || third > 10 then
                  Left $ InvalidRoll 20 third
                else
                  Right (total + first + second + third)
              _ -> Left $ InvalidRoll 20 (rest !! 1)  -- Too many rolls
