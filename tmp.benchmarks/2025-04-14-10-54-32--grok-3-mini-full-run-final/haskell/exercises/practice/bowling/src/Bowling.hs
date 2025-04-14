module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = scoreHelper 1 0 rolls 0
  where
    scoreHelper :: Int -> Int -> [Int] -> Int -> Either BowlingError Int
    scoreHelper frame index rollList totalScore
      | frame > 10 = Right totalScore  -- All frames processed
      | frame == 10 =                  -- Handle 10th frame specially
          if index >= length rollList
          then Left IncompleteGame     -- Not enough rolls
          else
            let firstRoll = rollList !! index
            in if firstRoll < 0 || firstRoll > 10
               then Left (InvalidRoll index firstRoll)  -- Invalid first roll
               else if firstRoll == 10  -- Strike in 10th frame
                    then
                      if index + 1 < length rollList && index + 2 < length rollList
                      then
                        let next1 = rollList !! (index + 1)
                            next2 = rollList !! (index + 2)
                        in if next1 < 0 || next1 > 10
                           then Left (InvalidRoll (index + 1) next1)
                           else if next2 < 0 || next2 > 10
                                then Left (InvalidRoll (index + 2) next2)
                                else Right (totalScore + 10 + next1 + next2)  -- Sum of 10th frame rolls
                      else Left IncompleteGame  -- Not enough rolls for bonus
                    else if index + 1 < length rollList
                         then
                           let secondRoll = rollList !! (index + 1)
                           in if secondRoll < 0 || secondRoll > 10
                              then Left (InvalidRoll (index + 1) secondRoll)
                              else if firstRoll + secondRoll > 10
                                   then Left (InvalidRoll (index + 1) secondRoll)  -- Invalid sum
                                   else if firstRoll + secondRoll == 10  -- Spare in 10th frame
                                        then if index + 2 < length rollList
                                             then
                                               let thirdRoll = rollList !! (index + 2)
                                               in if thirdRoll < 0 || thirdRoll > 10
                                                  then Left (InvalidRoll (index + 2) thirdRoll)
                                                  else Right (totalScore + 10 + thirdRoll)  -- Sum of 10th frame rolls
                                             else Left IncompleteGame  -- Not enough rolls for bonus
                                        else Right (totalScore + firstRoll + secondRoll)  -- Open frame
                         else Left IncompleteGame  -- Not enough rolls
      | otherwise =                   -- Frames 1 to 9
          if index >= length rollList
          then Left IncompleteGame     -- Not enough rolls
          else let roll1 = rollList !! index
               in if roll1 < 0 || roll1 > 10
                  then Left (InvalidRoll index roll1)  -- Invalid roll
                  else if roll1 == 10  -- Strike
                       then if index + 2 < length rollList
                            then let next1 = rollList !! (index + 1)
                                     next2 = rollList !! (index + 2)
                                 in if next1 < 0 || next1 > 10
                                    then Left (InvalidRoll (index + 1) next1)
                                    else if next2 < 0 || next2 > 10
                                         then Left (InvalidRoll (index + 2) next2)
                                         else scoreHelper (frame + 1) (index + 1) rollList (totalScore + 10 + next1 + next2)
                            else Left IncompleteGame
                       else if index + 1 < length rollList
                            then let roll2 = rollList !! (index + 1)
                                 in if roll2 < 0 || roll2 > 10
                                    then Left (InvalidRoll (index + 1) roll2)
                                    else if roll1 + roll2 > 10  -- Invalid for this frame
                                         then Left (InvalidRoll (index + 1) roll2)  -- Mark as invalid
                                         else if roll1 + roll2 == 10  -- Spare
                                              then if index + 2 < length rollList
                                                   then let next = rollList !! (index + 2)
                                                        in if next < 0 || next > 10
                                                           then Left (InvalidRoll (index + 2) next)
                                                           else scoreHelper (frame + 1) (index + 2) rollList (totalScore + 10 + next)
                                                   else Left IncompleteGame
                                              else scoreHelper (frame + 1) (index + 2) rollList (totalScore + roll1 + roll2)  -- Open frame
                            else Left IncompleteGame  -- Not enough rolls for the frame
