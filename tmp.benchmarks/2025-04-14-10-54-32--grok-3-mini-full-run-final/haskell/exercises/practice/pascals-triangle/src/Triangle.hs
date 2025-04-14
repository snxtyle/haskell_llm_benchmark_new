module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []  -- Return an empty list if 0 rows are requested
rows n = take n generateRows  -- Generate and take the first n rows

-- Helper function to generate rows iteratively
generateRows :: [[Integer]]
generateRows = iterate nextRow [1]  -- Start with the first row: [1]

-- Function to compute the next row from the previous one
nextRow :: [Integer] -> [Integer]
nextRow previous = zipWith (+) (0 : previous) (previous ++ [0])  -- Add edges and sum adjacent elements
