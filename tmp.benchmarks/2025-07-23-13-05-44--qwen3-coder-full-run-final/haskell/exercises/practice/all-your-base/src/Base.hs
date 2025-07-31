module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
    | inputBase < 2 = Left InvalidInputBase
    | outputBase < 2 = Left InvalidOutputBase
    | any (\d -> d < 0 || d >= inputBase) inputDigits = Left (InvalidDigit (head $ filter (\d -> d < 0 || d >= inputBase) inputDigits))
    | all (== 0) inputDigits = Right [0]
    | otherwise = Right $ convertToBase outputBase (fromBase inputBase inputDigits)

fromBase :: Integral a => a -> [a] -> a
fromBase base digits = sum $ zipWith (*) digits powers
  where
    powers = reverse $ take (length digits) $ map (base^) [0..]

convertToBase :: Integral a => a -> a -> [a]
convertToBase base 0 = [0]
convertToBase base num = reverse $ convertToBase' base num []
  where
    convertToBase' b n acc
        | n == 0 = acc
        | otherwise = convertToBase' b (n `div` b) ((n `mod` b) : acc)
