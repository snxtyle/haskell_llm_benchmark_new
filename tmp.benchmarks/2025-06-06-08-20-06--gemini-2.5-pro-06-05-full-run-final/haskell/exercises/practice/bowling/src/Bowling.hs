module Bowling (score, BowlingError(..)) where

import Control.Monad (zipWithM_)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
    -- First, validate that all individual roll values are between 0 and 10.
    zipWithM_ (\i r -> validateRollValue i r) [0..] rolls
    -- Then, start scoring the game from frame 1.
    game 1 0 rolls

-- A helper to validate a single roll's value.
validateRollValue :: Int -> Int -> Either BowlingError ()
validateRollValue idx val
  | val < 0 || val > 10 = Left (InvalidRoll idx val)
  | otherwise           = Right ()

-- `game` is the main recursive function for scoring.
-- It takes the current frame number, the index of the first roll of the current frame,
-- and the list of remaining rolls.
game :: Int -> Int -> [Int] -> Either BowlingError Int

-- Frame 10 has special scoring rules, handled by a separate function.
-- This is the base case for the recursion.
game 10 rollIdx rolls = scoreTenthFrame rollIdx rolls

-- Frames 1-9
game frame rollIdx (r1:rs)
  -- Strike: first roll of the frame is 10 pins.
  | r1 == 10 = do
      -- A strike's bonus is the next two rolls. Check if they exist.
      (r2, r3) <- case rs of
                    (x:y:_) -> Right (x, y)
                    _       -> Left IncompleteGame
      -- A strike consumes one roll. Recurse on the rest of the rolls for the next frame.
      restScore <- game (frame + 1) (rollIdx + 1) rs
      Right (10 + r2 + r3 + restScore)

  -- Not a strike
  | otherwise = case rs of
      [] -> Left IncompleteGame
      (r2:remainingRolls) ->
        if r1 + r2 > 10
          then Left (InvalidRoll (rollIdx + 1) r2)
          else do
            -- An open frame or spare consumes two rolls. Recurse for the next frame.
            restScore <- game (frame + 1) (rollIdx + 2) remainingRolls
            -- Spare: the two rolls sum to 10.
            if r1 + r2 == 10
              then do
                -- A spare's bonus is the next single roll. Check if it exists.
                bonus <- case remainingRolls of
                           (x:_) -> Right x
                           _     -> Left IncompleteGame
                Right (10 + bonus + restScore)
              -- Open Frame: the two rolls sum to less than 10.
              else Right (r1 + r2 + restScore)

-- Not enough rolls to complete a frame.
game _ _ [] = Left IncompleteGame

-- `scoreTenthFrame` handles the specific logic for the final frame.
scoreTenthFrame :: Int -> [Int] -> Either BowlingError Int
scoreTenthFrame rollIdx (r1:rs)
  -- Strike on the first ball of the 10th frame.
  | r1 == 10 = case rs of
      [r2, r3] ->
        if r2 /= 10 && r2 + r3 > 10
          then Left (InvalidRoll (rollIdx + 2) r3)
          else Right (10 + r2 + r3)
      (_:_:r4:_) -> Left (InvalidRoll (rollIdx + 3) r4)
      _ -> Left IncompleteGame

  -- Not a strike on the first ball.
  | otherwise = case rs of
      [] -> Left IncompleteGame
      (r2:rs') ->
        if r1 + r2 > 10
          then Left (InvalidRoll (rollIdx + 1) r2)
          else if r1 + r2 == 10 -- Spare
            then case rs' of
              [r3] -> Right (10 + r3)
              []   -> Left IncompleteGame
              (_:r4:_) -> Left (InvalidRoll (rollIdx + 3) r4)
            else -- Open frame
              case rs' of
                [] -> Right (r1 + r2)
                (r3:_) -> Left (InvalidRoll (rollIdx + 2) r3)

-- Not enough rolls to complete the 10th frame.
scoreTenthFrame _ [] = Left IncompleteGame
