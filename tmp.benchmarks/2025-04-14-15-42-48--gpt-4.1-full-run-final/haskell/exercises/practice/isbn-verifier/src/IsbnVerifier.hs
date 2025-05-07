module IsbnVerifier (isbn) where

import Data.Char (digitToInt, isDigit)

isbn :: String -> Bool
isbn s =
    let cleaned = filter (/= '-') s
    in case cleaned of
        -- Must be exactly 10 characters
        [a,b,c,d,e,f,g,h,i,j] ->
            let digits = mapM toIsbnDigit (zip [1..10] [a,b,c,d,e,f,g,h,i,j])
            in case digits of
                Just ds ->
                    let weights = [10,9..1]
                        checksum = sum $ zipWith (*) ds weights
                    in checksum `mod` 11 == 0
                Nothing -> False
        _ -> False

-- For positions 1-9, must be digit. For position 10, can be digit or 'X'
toIsbnDigit :: (Int, Char) -> Maybe Int
toIsbnDigit (pos, c)
    | pos < 10 && isDigit c = Just (digitToInt c)
    | pos == 10 && c == 'X' = Just 10
    | pos == 10 && isDigit c = Just (digitToInt c)
    | otherwise = Nothing
