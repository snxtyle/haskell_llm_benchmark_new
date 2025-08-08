module Bowling (score, BowlingError(..)) where

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

score :: [Int] -> Either BowlingError Int
score rolls = do
  validatedRolls <- validateRolls rolls
  frames <- parseFrames validatedRolls
  if length frames < 10
    then Left IncompleteGame
    else Right $ sum $ map frameScore $ zip [1..] frames
  where
    frameScore (frameNum, frame)
      | frameNum == 10 = sum frame  -- 10th frame: just sum all rolls
      | otherwise = case frame of
          [10] -> 10 + strikeBonus frameNum  -- Strike
          [a, b] | a + b == 10 -> 10 + spareBonus frameNum  -- Spare
          [a, b] -> a + b  -- Open frame
          _ -> 0  -- Should not happen with valid input
    
    strikeBonus frameNum = 
      let nextRolls = getRollsAfterFrame frameNum rolls
      in sum $ take 2 nextRolls
    
    spareBonus frameNum =
      let nextRolls = getRollsAfterFrame frameNum rolls
      in sum $ take 1 nextRolls

-- Validate that all rolls are between 0 and 10
validateRolls :: [Int] -> Either BowlingError [Int]
validateRolls rolls = 
  case filter (\(_, v) -> v < 0 || v > 10) $ zip [0..] rolls of
    [] -> Right rolls
    ((idx, val):_) -> Left $ InvalidRoll idx val

-- Parse rolls into frames
parseFrames :: [Int] -> Either BowlingError [[Int]]
parseFrames rolls = parseFramesHelper rolls 1 []
  where
    parseFramesHelper [] _ frames = Right $ reverse frames
    parseFramesHelper remaining frameNum frames
      | frameNum > 10 = Right $ reverse frames
      | frameNum == 10 = parseTenthFrame remaining frames
      | otherwise = parseRegularFrame remaining frameNum frames
    
    parseRegularFrame [] _ frames = Right $ reverse frames
    parseRegularFrame (r:rs) frameNum frames
      | r == 10 = parseFramesHelper rs (frameNum + 1) ([10] : frames)  -- Strike
      | otherwise = case rs of
          [] -> Right $ reverse ([r] : frames)  -- Incomplete frame at end
          (r2:rest) ->
            if r + r2 > 10
              then Left $ InvalidRoll (length rolls - length rs) r2
              else parseFramesHelper rest (frameNum + 1) ([r, r2] : frames)
    
    parseTenthFrame [] frames = Right $ reverse frames
    parseTenthFrame remaining frames =
      case remaining of
        [] -> Right $ reverse frames
        [a] -> Right $ reverse ([a] : frames)
        [a, b] | a == 10 -> Right $ reverse ([a, b] : frames)
        [a, b] | a + b >= 10 -> Right $ reverse ([a, b] : frames)
        [a, b] -> 
          if a + b > 10
            then Left $ InvalidRoll (length rolls - length remaining + 1) b
            else Right $ reverse ([a, b] : frames)
        (a:b:c:rest) | a == 10 -> Right $ reverse ([a, b, c] : frames)
        (a:b:c:rest) | a + b == 10 -> Right $ reverse ([a, b, c] : frames)
        (a:b:rest) -> 
          if a + b > 10
            then Left $ InvalidRoll (length rolls - length remaining + 1) b
            else Right $ reverse ([a, b] : frames)

-- Get rolls after a specific frame for bonus calculation
getRollsAfterFrame :: Int -> [Int] -> [Int]
getRollsAfterFrame targetFrame rolls = drop (rollsUsedInFrames targetFrame) rolls
  where
    rollsUsedInFrames frame = rollsUsedHelper rolls 1 0
    rollsUsedHelper [] _ count = count
    rollsUsedHelper _ f count | f > targetFrame = count
    rollsUsedHelper (r:rs) f count
      | f == 10 = length rolls  -- All rolls used by frame 10
      | r == 10 = rollsUsedHelper rs (f + 1) (count + 1)  -- Strike uses 1 roll
      | otherwise = case rs of
          [] -> count + 1
          (_:rest) -> rollsUsedHelper rest (f + 1) (count + 2)  -- Regular frame uses 2 rolls
