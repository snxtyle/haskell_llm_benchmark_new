module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase <= 1 = Left InvalidInputBase
  | outputBase <= 1 = Left InvalidOutputBase
  | otherwise =
      case firstInvalidDigit inputDigits of
        Just d  -> Left (InvalidDigit d)
        Nothing ->
          if null inputDigits
            then Right []
            else
              let bIn   = toInteger inputBase
                  bOut  = toInteger outputBase
                  value = foldl (\acc d -> acc * bIn + toInteger d) 0 inputDigits
              in if value == 0
                   then Right [0]
                   else Right (map fromInteger (toBase bOut value))
  where
    firstInvalidDigit [] = Nothing
    firstInvalidDigit (d:ds)
      | d < 0 || d >= inputBase = Just d
      | otherwise               = firstInvalidDigit ds

    toBase :: Integer -> Integer -> [Integer]
    toBase b n = reverse (go n)
      where
        go 0 = []
        go m = let (q, r) = m `divMod` b
               in r : go q
