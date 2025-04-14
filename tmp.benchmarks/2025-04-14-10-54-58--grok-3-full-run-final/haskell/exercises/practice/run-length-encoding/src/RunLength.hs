module RunLength (decode, encode) where

decode :: String -> String
decode encodedText = 
    let (result, rest) = decodeHelper encodedText "" 0
    in result ++ rest
  where
    decodeHelper [] acc _ = (acc, "")
    decodeHelper (x:xs) acc num
        | x >= '0' && x <= '9' = decodeHelper xs acc (num * 10 + (fromEnum x - fromEnum '0'))
        | num == 0 = decodeHelper xs (acc ++ [x]) 0
        | otherwise = decodeHelper xs (acc ++ replicate num x) 0

encode :: String -> String
encode text = encodeHelper text "" 1
  where
    encodeHelper [] acc _ = acc
    encodeHelper [x] acc count
        | count == 1 = acc ++ [x]
        | otherwise = acc ++ show count ++ [x]
    encodeHelper (x:y:xs) acc count
        | x == y = encodeHelper (y:xs) acc (count + 1)
        | count == 1 = encodeHelper (y:xs) (acc ++ [x]) 1
        | otherwise = encodeHelper (y:xs) (acc ++ show count ++ [x]) 1
