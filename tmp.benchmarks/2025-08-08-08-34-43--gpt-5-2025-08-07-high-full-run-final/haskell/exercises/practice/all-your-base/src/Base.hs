module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase <= 1  = Left InvalidInputBase
  | outputBase <= 1 = Left InvalidOutputBase
  | otherwise =
      case firstInvalidDigit inputBase inputDigits of
        Just d  -> Left (InvalidDigit d)
        Nothing -> Right (toBase outputBase (fromBase inputBase inputDigits))
  where
    firstInvalidDigit :: Integral a => a -> [a] -> Maybe a
    firstInvalidDigit b = go
      where
        go [] = Nothing
        go (d:ds)
          | d < 0     = Just d
          | d >= b    = Just d
          | otherwise = go ds

    fromBase :: Integral a => a -> [a] -> a
    fromBase b = foldl (\acc d -> acc * b + d) 0

    toBase :: Integral a => a -> a -> [a]
    toBase b n
      | n == 0    = []
      | otherwise = reverse (unfold n)
      where
        unfold 0 = []
        unfold x = let (q, r) = x `divMod` b in r : unfold q
