module Change (findFewestCoins) where

infinity :: Integer
infinity = 999999999999  -- A large number representing infinity for DP

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target <= 0 = Nothing  -- Negative or zero target is invalid
  | otherwise =
      let validCoins = filter (> 0) coins  -- Only consider positive coins
      in if null validCoins
         then Nothing  -- No valid coins available
         else
           let targetInt = fromIntegral target :: Int  -- Assume target fits in Int for indexing
               dpList = [0] ++ [ minimum [ if c <= fromIntegral i && i - fromIntegral c >= 0
                                           then 1 + (dpList !! fromIntegral (i - fromIntegral c))
                                           else infinity  -- Use infinity as a large number
                                         | c <- validCoins
                                         ]
                               | i <- [1..targetInt]
                       ]  -- Dynamic programming list
           in if dpList !! targetInt == infinity
              then Nothing  -- Impossible to make the amount
              else Just (backtrack dpList validCoins targetInt)

-- Helper function to backtrack and get the coin list
backtrack :: [Integer] -> [Integer] -> Int -> [Integer]
backtrack dp coins amount
  | amount == 0 = []  -- Base case: no amount left
  | otherwise =
      let candidates = [ c | c <- coins  -- Find coins that lead to the minimum
                           , c <= fromIntegral amount
                           , 1 + (dp !! fromIntegral (amount - fromIntegral c)) == dp !! amount
                           ]
      in case candidates of
           (c:_) -> c : backtrack dp coins (fromIntegral (amount - fromIntegral c))  -- Recur with the first valid coin
           []    -> []  -- Should not happen if dp is correct
