module BookStore (total, Book(..)) where

data Book = First | Second | Third | Fourth | Fifth
  deriving (Eq, Ord, Show)

total :: [Book] -> Int
total basket = minPrice (frequencies basket)
  where
    frequencies :: [Book] -> [Int]
    frequencies books = map (\b -> length $ filter (==b) books) [First, Second, Third, Fourth, Fifth]
    
    minPrice :: [Int] -> Int
    minPrice counts = minPrice' (filter (>0) counts)
    
    minPrice' :: [Int] -> Int
    minPrice' [] = 0
    minPrice' counts = 
      let maxGroupSize = min 5 (length counts)
          prices = [calculateDiscount groupSize + minPrice' (decrementFirstN groupSize (sortDesc counts)) 
                   | groupSize <- [1..maxGroupSize]]
      in minimum prices
    
    decrementFirstN :: Int -> [Int] -> [Int]
    decrementFirstN 0 xs = xs
    decrementFirstN _ [] = []
    decrementFirstN n (x:xs) = 
      let decremented = max 0 (x - 1)
      in if decremented > 0 
         then decremented : decrementFirstN (n-1) xs
         else decrementFirstN (n-1) xs
    
    sortDesc :: [Int] -> [Int]
    sortDesc [] = []
    sortDesc (x:xs) = sortDesc larger ++ [x] ++ sortDesc smaller
      where
        smaller = filter (<= x) xs
        larger = filter (> x) xs
    
    calculateDiscount :: Int -> Int
    calculateDiscount n = n * 800 * (100 - discount n) `div` 100
    
    discount :: Int -> Int
    discount 1 = 0
    discount 2 = 5
    discount 3 = 10
    discount 4 = 20
    discount 5 = 25
    discount _ = 0
