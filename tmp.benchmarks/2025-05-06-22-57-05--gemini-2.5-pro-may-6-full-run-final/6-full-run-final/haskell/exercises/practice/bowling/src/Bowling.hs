module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = processFrames 1 0 rolls
  where
    -- Helper to validate a single roll's value (0-10 pins).
    validateRollValue :: Int -> Int -> Either BowlingError ()
    validateRollValue idx val
      | val < 0 || val > 10 = Left InvalidRoll { rollIndex = idx, rollValue = val }
      | otherwise           = Right ()

    -- processFrames calculates the score recursively, frame by frame.
    -- frameNum: The current frame number being processed (1 to 10).
    -- currentRollIdx: The index in the original 'rolls' list corresponding to the start of 'remainingRolls'.
    -- remainingRolls: The list of rolls from the current frame onwards.
    processFrames :: Int -> Int -> [Int] -> Either BowlingError Int
    processFrames frameNum currentRollIdx remainingRolls

      -- Base case: All 10 frames have been processed.
      | frameNum > 10 =
          if null remainingRolls
          then Right 0 -- Successfully processed 10 frames and no rolls left. Score contribution is 0.
          else Left IncompleteGame -- Rolls remain after 10 frames, but not caught by 10th frame logic; game structure is invalid.

      -- Logic for the 10th (final) frame.
      | frameNum == 10 =
          case remainingRolls of
            -- Must have at least two rolls for the 10th frame.
            r1:r2:restOfRollsAfterTwo -> do
              _ <- validateRollValue currentRollIdx r1
              _ <- validateRollValue (currentRollIdx + 1) r2

              if r1 == 10 then do -- Strike in the 10th frame (X r2 r3)
                case restOfRollsAfterTwo of
                  r3:finalRest -> do
                    _ <- validateRollValue (currentRollIdx + 2) r3
                    -- If the first fill ball (r2) is not a strike, then r2 + r3 must not exceed 10
                    -- (as they effectively form a "frame" after the initial strike).
                    _ <- if r2 < 10 && r2 + r3 > 10
                         then Left InvalidRoll { rollIndex = currentRollIdx + 2, rollValue = r3 }
                         else Right ()
                    if not (null finalRest)
                      -- Too many rolls after the 10th frame's three rolls.
                      -- The first extra roll is at index currentRollIdx + 3.
                      then Left InvalidRoll { rollIndex = currentRollIdx + 3, rollValue = head finalRest }
                      else Right (r1 + r2 + r3) -- Score for 10th frame is sum of its three rolls.
                  [] -> Left IncompleteGame -- Strike in 10th, but missing the third roll (r3).

              else if r1 + r2 == 10 then do -- Spare in the 10th frame (r1 / r3), r1 must be < 10.
                case restOfRollsAfterTwo of
                  r3:finalRest -> do
                    _ <- validateRollValue (currentRollIdx + 2) r3
                    if not (null finalRest)
                      -- Too many rolls after the 10th frame's three rolls.
                      -- The first extra roll is at index currentRollIdx + 3.
                      then Left InvalidRoll { rollIndex = currentRollIdx + 3, rollValue = head finalRest }
                      else Right (r1 + r2 + r3) -- Score for 10th frame is sum of its three rolls.
                  [] -> Left IncompleteGame -- Spare in 10th, but missing the third roll (r3).

              else if r1 + r2 < 10 then do -- Open frame in the 10th (r1 r2), r1 must be < 10.
                if not (null restOfRollsAfterTwo)
                  -- Too many rolls for an open 10th frame.
                  -- The first extra roll is at index currentRollIdx + 2.
                  then Left InvalidRoll { rollIndex = currentRollIdx + 2, rollValue = head restOfRollsAfterTwo }
                  else Right (r1 + r2) -- Score is sum of the two rolls.
              else -- This case means r1 < 10 and r1 + r2 > 10. Invalid roll.
                  Left InvalidRoll { rollIndex = currentRollIdx + 1, rollValue = r2 }

            _ -> Left IncompleteGame -- Not enough rolls for the 10th frame (needs at least r1, r2).


      -- Logic for frames 1-9.
      | otherwise = -- frameNum is < 10
          case remainingRolls of
            r1:rollsAfterR1 -> do
              _ <- validateRollValue currentRollIdx r1
              if r1 == 10 then do -- Strike in frames 1-9
                -- A strike needs two subsequent rolls for its bonus calculation.
                case rollsAfterR1 of
                  b1:b2:_ -> do
                    _ <- validateRollValue (currentRollIdx + 1) b1 -- Validate bonus rolls individually.
                    _ <- validateRollValue (currentRollIdx + 2) b2
                    let strikeScore = 10 + b1 + b2
                    -- A strike frame consumes 1 roll from 'remainingRolls' for frame advancement.
                    remainingFramesScore <- processFrames (frameNum + 1) (currentRollIdx + 1) rollsAfterR1
                    Right (strikeScore + remainingFramesScore)
                  _ -> Left IncompleteGame -- Not enough rolls available for strike bonus.

              else do -- Not a strike (r1 < 10), so it's an open frame or a spare. Needs a second roll.
                case rollsAfterR1 of
                  r2:rollsAfterR2 -> do
                    _ <- validateRollValue (currentRollIdx + 1) r2
                    -- Check if the sum of two rolls in the frame is valid.
                    if r1 + r2 > 10 then Left InvalidRoll { rollIndex = currentRollIdx + 1, rollValue = r2 }
                    else do
                      if r1 + r2 == 10 then do -- Spare in frames 1-9
                        -- A spare needs one subsequent roll for its bonus.
                        case rollsAfterR2 of
                          b1:_ -> do
                            _ <- validateRollValue (currentRollIdx + 2) b1
                            let spareScore = 10 + b1
                            -- A spare frame consumes 2 rolls from 'remainingRolls' for frame advancement.
                            remainingFramesScore <- processFrames (frameNum + 1) (currentRollIdx + 2) rollsAfterR2
                            Right (spareScore + remainingFramesScore)
                          _ -> Left IncompleteGame -- Not enough rolls available for spare bonus.
                      else do -- Open frame in frames 1-9 (r1 + r2 < 10)
                        let openFrameScore = r1 + r2
                        -- An open frame consumes 2 rolls from 'remainingRolls' for frame advancement.
                        remainingFramesScore <- processFrames (frameNum + 1) (currentRollIdx + 2) rollsAfterR2
                        Right (openFrameScore + remainingFramesScore)
                  _ -> Left IncompleteGame -- Not enough rolls for the second roll of an open/spare frame.
            [] -> Left IncompleteGame -- No rolls left for the current frame (1-9).
