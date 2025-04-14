module IsbnVerifier (isbn) where

isbn :: String -> Bool
isbn str = 
    let 
        -- Remove hyphens from the string
        cleaned = filter (/= '-') str
        -- Check if length is 10
        validLength = length cleaned == 10
        -- Check if all characters except possibly the last are digits
        validChars = all (\(c, pos) -> if pos == 9 then (c >= '0' && c <= '9') || c == 'X' else (c >= '0' && c <= '9')) (zip cleaned [0..9])
        -- Convert characters to their numeric values (X = 10)
        digits = map (\c -> if c == 'X' then 10 else read [c] :: Int) cleaned
        -- Calculate the weighted sum
        weightedSum = sum $ zipWith (*) digits [10,9..1]
    in
        validLength && validChars && weightedSum `mod` 11 == 0
