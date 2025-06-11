module Base (Error(..), rebase) where

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase < 2 = Left InvalidInputBase
  | outputBase < 2 = Left InvalidOutputBase
  | otherwise = 
      case findInvalid inputDigits of
        Just d -> Left (InvalidDigit d)
        Nothing -> 
          if null inputDigits
          then Right []
          else let n = foldl (\acc x -> acc * inputBase + x) 0 inputDigits
               in Right (convert outputBase n)
  where 
    findInvalid [] = Nothing
    findInvalid (d:ds)
      | d < 0 || d >= inputBase = Just d
      | otherwise = findInvalid ds

    convert _ 0 = [0]
    convert base n = 
      let (q, r) = n `divMod` base
      in if q == 0
         then [r]
         else convert base q ++ [r]
