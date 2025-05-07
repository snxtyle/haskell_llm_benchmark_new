module Bowling (score, BowlingError(..)) where

import Control.Monad (unless)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

-- Calculates the score of a bowling game given a list of rolls.
score :: [Int] -> Either BowlingError Int
score rolls =
  let n = length rolls
      -- Helper to safely get and validate a roll value at a given index.
      -- Checks for index out of bounds (IncompleteGame) and invalid pin count (InvalidRoll).
      getRoll :: Int -> Either BowlingError Int
      getRoll idx =
        if idx >= n then Left IncompleteGame
        else let value = rolls !! idx
             in if value < 0 || value > 10
                then Left InvalidRoll { rollIndex = idx, rollValue = value }
                else Right value
  -- First, validate the entire game structure and roll values.
  in case validateFrames 1 0 getRoll n rolls of -- Pass 'rolls' to validateFrames
       Left err -> Left err
       -- If validation passes (Right ()), calculate the score.
       Right () -> Right (calculateScore 1 0 rolls)

-- Validates the sequence of rolls for game structure and rules (10 frames).
-- Takes the current frame number, current roll index, the getRoll helper, total number of rolls,
-- and the rolls list itself (needed for the final check).
-- Returns Right () if the game structure is valid, Left BowlingError otherwise.
validateFrames :: Int -> Int -> (Int -> Either BowlingError Int) -> Int -> [Int] -> Either BowlingError ()
validateFrames frame rollIdx getRoll n rolls = -- Added 'rolls' parameter
  if frame > 10 then
    -- After processing 10 frames, check if the correct number of rolls were consumed.
    if rollIdx == n then
      Right () -- Exactly the right number of rolls consumed for 10 frames.
    else if rollIdx < n then
      -- Too many rolls provided. The game finished using rolls up to index rollIdx-1.
      -- Report the first extra roll (at rollIdx) as invalid.
      -- We need its value for the error message. Use getRoll to validate its value too.
      case getRoll rollIdx of
        Right val -> Left InvalidRoll { rollIndex = rollIdx, rollValue = val }
        -- If the extra roll itself has an invalid value (e.g. > 10), getRoll returns
        -- InvalidRoll. Propagate that error.
        Left err -> Left err
    else -- rollIdx > n
      -- This implies the logic expected more rolls than were provided for the 10 frames.
      -- This should have been caught earlier by 'getRoll' calls within the frame processing.
      Left IncompleteGame
  else do
    -- Get the first roll of the current frame. getRoll handles bounds and value checks.
    r1 <- getRoll rollIdx
    if r1 == 10 then do -- Strike
      -- A strike requires two subsequent rolls for bonus calculation.
      -- We check if they exist using getRoll. Their values will be validated if needed (e.g., in frame 10).
      -- Note: These checks also prevent reading past the end if a strike occurs near the end
      -- of an incomplete roll list.
      _ <- getRoll (rollIdx + 1)
      _ <- getRoll (rollIdx + 2)

      if frame == 10 then do
         -- Special validation for 10th frame strike fill balls.
         r2 <- getRoll (rollIdx + 1) -- The first fill roll.
         r3 <- getRoll (rollIdx + 2) -- The second fill roll.
         -- If the first fill roll (r2) is not a strike (i.e., < 10),
         -- then the sum of the two fill rolls (r2 + r3) must not exceed 10.
         -- This validation is only needed if r2 is not a strike.
         unless (r2 == 10 || r2 + r3 <= 10) $
           Left InvalidRoll { rollIndex = rollIdx + 2, rollValue = r3 }
         -- A strike in the 10th frame, along with its two fill balls, completes the game.
         -- Advance roll index by 3 and check the next frame (which should trigger the base case).
         validateFrames (frame + 1) (rollIdx + 3) getRoll n rolls
      else
         -- A strike in frames 1-9 consumes 1 roll position for this frame's calculation.
         -- Move to the next frame, advancing the roll index by 1.
         validateFrames (frame + 1) (rollIdx + 1) getRoll n rolls
    else do -- Not a strike (Open frame or Spare)
      -- Need the second roll for the frame. getRoll handles bounds/value checks.
      r2 <- getRoll (rollIdx + 1)
      -- The sum of the two rolls in a frame cannot exceed 10.
      unless (r1 + r2 <= 10) $
        Left InvalidRoll { rollIndex = rollIdx + 1, rollValue = r2 }

      if frame == 10 then
        -- Handle 10th frame specifically.
        if r1 + r2 == 10 then do -- Spare in 10th frame
          -- A spare requires one fill ball. Check if it exists and validate its value.
          _ <- getRoll (rollIdx + 2)
          -- A spare in the 10th frame, along with its fill ball, completes the game.
          -- Advance roll index by 3 and check the next frame (base case).
          validateFrames (frame + 1) (rollIdx + 3) getRoll n rolls
        else -- Open frame in 10th frame
          -- An open frame in the 10th consumes 2 rolls and completes the game.
          -- Advance roll index by 2 and check the next frame (base case).
          validateFrames (frame + 1) (rollIdx + 2) getRoll n rolls
      else -- Frames 1-9, Open or Spare
        -- An open frame or spare in frames 1-9 consumes 2 roll positions for this frame.
        -- Move to the next frame, advancing the roll index by 2.
        validateFrames (frame + 1) (rollIdx + 2) getRoll n rolls

-- Calculates the score recursively, assuming the rolls list represents a valid game.
-- Takes the current frame number, current roll index, and the list of all rolls.
-- Returns the total score for frames `frame` through 10.
calculateScore :: Int -> Int -> [Int] -> Int
calculateScore frame rollIdx rolls =
  if frame > 10 then 0 -- Base case: score calculation stops after frame 10.
  else
    let r1 = rolls !! rollIdx -- First roll of the current frame.
    in if r1 == 10 then -- Strike
         -- Score = 10 + bonus (next two rolls) + score of subsequent frames.
         -- Accessing rollIdx+1 and rollIdx+2 is safe due to prior validation.
         let bonus = (rolls !! (rollIdx + 1)) + (rolls !! (rollIdx + 2))
         in (10 + bonus) + calculateScore (frame + 1) (rollIdx + 1) rolls
       else -- Not a strike (Open frame or Spare)
         -- Accessing rollIdx+1 is safe due to prior validation.
         let r2 = rolls !! (rollIdx + 1) -- Second roll of the current frame.
             framePins = r1 + r2 -- Pins knocked down in this frame's rolls.
         in if framePins == 10 then -- Spare
              -- Score = 10 + bonus (next one roll) + score of subsequent frames.
              -- Accessing rollIdx+2 is safe due to prior validation.
              let bonus = rolls !! (rollIdx + 2)
              in (10 + bonus) + calculateScore (frame + 1) (rollIdx + 2) rolls
            else -- Open frame
              -- Score = pins in this frame + score of subsequent frames.
              framePins + calculateScore (frame + 1) (rollIdx + 2) rolls
