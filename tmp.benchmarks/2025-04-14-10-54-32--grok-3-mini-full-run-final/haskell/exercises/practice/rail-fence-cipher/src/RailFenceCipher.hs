module RailFenceCipher (encode, decode) where

encode :: Int -> String -> String
encode n str
  | n <= 1 = str
  | otherwise = 
      let l = length str
          period = 2 * (n - 1)
          railFor i = let pos = i `mod` period in if pos < n then pos else period - pos
          railsList = [railFor i | i <- [0..l-1]]
          updatedRails = foldl (\rails i -> 
                                let r = railsList !! i
                                in take r rails ++ [rails !! r ++ [str !! i]] ++ drop (r + 1) rails) 
                               (replicate n "") [0..l-1]
      in concat updatedRails

decode :: Int -> String -> String
decode n encodedStr
  | n <= 1 = encodedStr
  | otherwise = 
      let l = length encodedStr
          period = 2 * (n - 1)
          railFor i = let pos = i `mod` period in if pos < n then pos else period - pos
          railsList = [railFor i | i <- [0..l-1]]
          counts = [length (filter (== k) railsList) | k <- [0..n-1]]
          railStrings = snd $ foldl (\(s, acc) c -> let (part, rest) = splitAt c s in (rest, acc ++ [part])) (encodedStr, []) counts
          (finalAcc, _) = foldl (\(acc, states) r -> 
                                 let (char:rest) = states !! r
                                 in (acc ++ [char], take r states ++ [rest] ++ drop (r + 1) states)) 
                                ("", railStrings) railsList
      in finalAcc
