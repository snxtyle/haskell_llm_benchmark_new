module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score allRolls =
    let n = length allRolls

        -- Helper to safely get a roll value by index and validate it.
        -- Returns Nothing if the index is out of bounds.
        -- Returns Just (Left err) if the roll value is invalid (e.g., < 0 or > 10).
        -- Returns Just (Right val) if the roll value is valid.
        getValidatedRoll :: Int -> Maybe (Either BowlingError Int)
        getValidatedRoll idx
            | idx < 0 || idx >= n = Nothing
            | otherwise =
                let r = allRolls !! idx
                in if r >= 0 && r <= 10
                   then Just (Right r)
                   else Just (Left (InvalidRoll idx r))

        -- Recursive function to calculate the score frame by frame.
        -- Parameters:
        --   rollIdx: The index of the current roll being processed in `allRolls`.
        --   frameNum: The current frame number (from 1 to 10).
        --   currentScore: The accumulated score so far.
        calculate :: Int -> Int -> Int -> Either BowlingError Int
        calculate rollIdx frameNum currentScore
            -- Base case: All 10 frames have been processed.
            | frameNum > 10 =
                -- If there are any rolls remaining after 10 frames, it indicates an invalid game state
                -- (too many rolls provided for a complete game).
                if rollIdx == n then Right currentScore
                else Left IncompleteGame -- Game is complete, but there are extra, unneeded rolls.

            -- Recursive step: Process the current frame.
            | otherwise =
                case getValidatedRoll rollIdx of
                    Nothing -> Left IncompleteGame -- Not enough rolls to start this frame.

                    Just (Left err) -> Left err    -- Invalid roll value for the first ball of the frame.

                    Just (Right r1) ->
                        if r1 == 10 -- Strike (10 pins knocked down on the first ball)
                        then
                            -- A strike requires the next two rolls for bonus points.
                            case (getValidatedRoll (rollIdx + 1), getValidatedRoll (rollIdx + 2)) of
                                (Just (Right r2), Just (Right r3)) ->
                                    -- Special validation for 10th frame bonus rolls:
                                    -- If it's the 10th frame and neither bonus roll is a strike,
                                    -- their sum cannot exceed 10.
                                    if frameNum == 10 && r2 < 10 && r3 < 10 && r2 + r3 > 10
                                    then Left (InvalidRoll (rollIdx + 2) r3) -- The second bonus roll makes the sum invalid
                                    else
                                        let frameScore = 10 + r2 + r3
                                        -- For frames 1-9, a strike consumes 1 roll from the `allRolls` list.
                                        -- For frame 10, a strike consumes 1 roll (the strike itself) plus 2 bonus rolls,
                                        -- effectively advancing the `rollIdx` by 3 for the entire frame's contribution.
                                            nextRollIdx = if frameNum == 10 then rollIdx + 3 else rollIdx + 1
                                        in calculate nextRollIdx (frameNum + 1) (currentScore + frameScore)
                                (Just (Left err), _) -> Left err -- Invalid bonus roll 1.
                                (_, Just (Left err)) -> Left err -- Invalid bonus roll 2.
                                _ -> Left IncompleteGame -- Not enough rolls available for strike bonus.

                        else -- Not a strike, so we need a second roll for the frame.
                            case getValidatedRoll (rollIdx + 1) of
                                Nothing -> Left IncompleteGame -- Not enough rolls for the second ball of the frame.
                                Just (Left err) -> Left err    -- Invalid roll value for the second ball.

                                Just (Right r2) ->
                                    -- Check for an invalid frame sum (e.g., 5 + 6 = 11, which is not a strike).
                                    if r1 + r2 > 10
                                    then Left (InvalidRoll (rollIdx + 1) r2) -- The second roll makes the sum invalid.
                                    else
                                        let frameScore = r1 + r2
                                        in if frameScore == 10 -- Spare (10 pins knocked down on two balls)
                                           then
                                               -- A spare requires the next roll for bonus points.
                                               case getValidatedRoll (rollIdx + 2) of
                                                   Nothing -> Left IncompleteGame -- Not enough rolls for spare bonus.
                                                   Just (Left err) -> Left err    -- Invalid bonus roll.
                                                   Just (Right r3) ->
                                                       let bonusScore = r3
                                                       -- For frames 1-9, a spare consumes 2 rolls from the `allRolls` list.
                                                       -- For frame 10, a spare consumes 2 rolls (the spare itself) plus 1 bonus roll,
                                                       -- effectively advancing the `rollIdx` by 3 for the entire frame's contribution.
                                                           nextRollIdx = if frameNum == 10 then rollIdx + 3 else rollIdx + 2
                                                       in calculate nextRollIdx (frameNum + 1) (currentScore + frameScore + bonusScore)
                                           else -- Open frame (less than 10 pins knocked down on two balls)
                                               -- An open frame consumes 2 rolls.
                                               let nextRollIdx = rollIdx + 2
                                               in calculate nextRollIdx (frameNum + 1) (currentScore + frameScore)

    -- Start the calculation from the first roll (index 0), first frame (frame 1), with an initial score of 0.
    in calculate 0 1 0
