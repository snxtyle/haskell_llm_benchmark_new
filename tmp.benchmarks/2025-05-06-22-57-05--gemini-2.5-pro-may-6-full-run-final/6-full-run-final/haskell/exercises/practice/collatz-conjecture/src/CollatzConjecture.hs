module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just (calculateSteps n 0)
  where
    calculateSteps :: Integer -> Integer -> Integer
    calculateSteps 1 currentSteps = currentSteps
    calculateSteps currentNum currentSteps
      | even currentNum = calculateSteps (currentNum `div` 2) (currentSteps + 1)
      | odd currentNum  = calculateSteps (3 * currentNum + 1) (currentSteps + 1)
