module Yacht (yacht, Category(..)) where

data Category = Ones
              | Twos
              | Threes
              | Fours
              | Fives
              | Sixes
              | FullHouse
              | FourOfAKind
              | LittleStraight
              | BigStraight
              | Choice
              | Yacht

yacht :: Category -> [Int] -> Int
yacht category dice = case category of
    Ones -> countDice 1 * 1
    Twos -> countDice 2 * 2
    Threes -> countDice 3 * 3
    Fours -> countDice 4 * 4
    Fives -> countDice 5 * 5
    Sixes -> countDice 6 * 6
    FullHouse -> scoreFullHouse
    FourOfAKind -> scoreFourOfAKind
    LittleStraight -> scoreLittleStraight
    BigStraight -> scoreBigStraight
    Choice -> sum dice
    Yacht -> scoreYacht
  where
    -- Count occurrences of a specific number in dice
    countDice n = length $ filter (== n) dice
    
    -- Sort dice for straight checks
    sortedDice = sort dice
    
    -- Check if all dice are the same
    scoreYacht = if all (== head dice) dice then 50 else 0
    
    -- Check for full house (three of one number and two of another)
    scoreFullHouse = 
      let counts = map (\x -> countDice x) [1..6]
          hasThree = 3 `elem` counts
          hasTwo = 2 `elem` counts
      in if hasThree && hasTwo then sum dice else 0
    
    -- Check for four of a kind
    scoreFourOfAKind = 
      let counts = map (\x -> countDice x) [1..6]
          maxCount = maximum counts
      in if maxCount >= 4 
         then let value = head $ filter (\x -> countDice x >= 4) [1..6]
              in value * 4
         else 0
    
    -- Check for little straight (1-2-3-4-5)
    scoreLittleStraight = if sortedDice == [1,2,3,4,5] then 30 else 0
    
    -- Check for big straight (2-3-4-5-6)
    scoreBigStraight = if sortedDice == [2,3,4,5,6] then 30 else 0

-- Helper function for sorting
sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = sort smaller ++ [x] ++ sort larger
  where
    smaller = filter (<= x) xs
    larger = filter (> x) xs
