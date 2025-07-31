module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0    = Nothing
  | n == 1    = Just Deficient
  | otherwise =
      let s = aliquotSum n
      in Just $ case compare s n of
        LT -> Deficient
        EQ -> Perfect
        GT -> Abundant

-- Compute the aliquot sum: sum of proper divisors of n (excluding n itself).
aliquotSum :: Int -> Int
aliquotSum n =
  let root = floor (sqrt (fromIntegral n :: Double))
      -- Start with 1 as a proper divisor for n > 1
      base = 1
      -- sum pairs (d, n/d) for d from 2 to root
      pairSum acc d
        | n `mod` d /= 0 = acc
        | otherwise =
            let q = n `div` d
            in if d == q
                 then acc + d        -- perfect square: add d only once
                 else acc + d + q    -- add both divisors
      summed = foldl pairSum base [2 .. root]
  in summed
