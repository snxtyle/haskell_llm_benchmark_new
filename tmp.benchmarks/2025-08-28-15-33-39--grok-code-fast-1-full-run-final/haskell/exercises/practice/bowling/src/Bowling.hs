module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = parse rolls 0 1 0 >>= \(total, remaining) ->
  if null remaining then Right total else Left IncompleteGame

parse :: [Int] -> Int -> Int -> Int -> Either BowlingError (Int, [Int])
parse rolls rollIdx frame scoreSoFar
  | frame > 10 =
      if null rolls
        then Right (scoreSoFar, rolls)
        else Left (InvalidRoll rollIdx (head rolls))
  | frame == 10 = parseTenth rolls rollIdx scoreSoFar
  | otherwise = parseRegular rolls rollIdx frame scoreSoFar

parseTenth :: [Int] -> Int -> Int -> Either BowlingError (Int, [Int])
parseTenth rolls rollIdx scoreSoFar
  | null rolls = Left IncompleteGame
  | otherwise =
      let r1 = head rolls
          rolls1 = tail rolls
          rollIdx1 = rollIdx + 1
      in if r1 < 0 || r1 > 10
           then Left (InvalidRoll rollIdx r1)
           else if r1 == 10  -- strike
                  then if null rolls1
                         then Left IncompleteGame
                         else let r2 = head rolls1
                                  rolls2 = tail rolls1
                                  rollIdx2 = rollIdx1 + 1
                              in if r2 < 0 || r2 > 10
                                   then Left (InvalidRoll rollIdx1 r2)
                                   else if null rolls2
                                          then Left IncompleteGame
                                          else let r3 = head rolls2
                                                   rolls3 = tail rolls2
                                                   rollIdx3 = rollIdx2 + 1
                                               in if r3 < 0 || r3 > 10
                                                    then Left (InvalidRoll rollIdx3 r3)
                                                    else if r2 + r3 > 10
                                                           then Left (InvalidRoll rollIdx3 r3)
                                                           else let totalScore = scoreSoFar + r1 + r2 + r3
                                                                in if null rolls3
                                                                     then Right (totalScore, rolls3)
                                                                     else Left (InvalidRoll rollIdx3 (head rolls3))
                else  -- not strike
                  if null rolls1
                    then Left IncompleteGame
                    else let r2 = head rolls1
                             rolls2 = tail rolls1
                             rollIdx2 = rollIdx1 + 1
                         in if r2 < 0 || r2 > 10
                              then Left (InvalidRoll rollIdx1 r2)
                              else if r1 + r2 > 10
                                     then Left (InvalidRoll rollIdx1 r2)
                                     else let score' = scoreSoFar + r1 + r2
                                          in if r1 + r2 == 10  -- spare
                                               then if null rolls2
                                                      then Left IncompleteGame
                                                      else let r3 = head rolls2
                                                               rolls3 = tail rolls2
                                                               rollIdx3 = rollIdx2 + 1
                                                           in if r3 < 0 || r3 > 10
                                                                then Left (InvalidRoll rollIdx3 r3)
                                                                else let totalScore = score' + r3
                                                                     in if null rolls3
                                                                          then Right (totalScore, rolls3)
                                                                          else Left (InvalidRoll rollIdx3 (head rolls3))
                                               else  -- open
                                                 if null rolls2
                                                   then Right (score', rolls2)
                                                   else Left (InvalidRoll rollIdx2 (head rolls2))

parseRegular :: [Int] -> Int -> Int -> Int -> Either BowlingError (Int, [Int])
parseRegular rolls rollIdx frame scoreSoFar
  | null rolls = Left IncompleteGame
  | otherwise =
      let r1 = head rolls
          rolls1 = tail rolls
          rollIdx1 = rollIdx + 1
      in if r1 < 0 || r1 > 10
           then Left (InvalidRoll rollIdx r1)
           else if r1 == 10  -- strike
                  then if length rolls < 3
                         then Left IncompleteGame
                         else let bonus = rolls !! 1 + rolls !! 2
                              in parse rolls1 rollIdx1 (frame + 1) (scoreSoFar + 10 + bonus)
                else  -- not strike
                  if null rolls1
                    then Left IncompleteGame
                    else let r2 = head rolls1
                             rolls2 = tail rolls1
                             rollIdx2 = rollIdx1 + 1
                         in if r2 < 0 || r2 > 10
                              then Left (InvalidRoll rollIdx1 r2)
                              else if r1 + r2 > 10
                                     then Left (InvalidRoll rollIdx1 r2)
                                     else let frameScore = r1 + r2
                                              score' = scoreSoFar + frameScore
                                          in if r1 + r2 == 10  -- spare
                                               then if null rolls2
                                                      then Left IncompleteGame
                                                      else let bonus = head rolls2
                                                           in if bonus < 0 || bonus > 10
                                                                then Left (InvalidRoll rollIdx2 bonus)
                                                                else parse rolls2 (rollIdx2 + 1) (frame + 1) (score' + bonus)
                                               else  -- open
                                                 parse rolls2 rollIdx2 (frame + 1) score'
