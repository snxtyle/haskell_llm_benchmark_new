module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs =
    let isValid c = c `elem` "ACGT"
        invalidChars = [c | c <- xs, not (isValid c)]
    in if null invalidChars
       then Right (map transcribe xs)
       else Left (head invalidChars)
  where
    transcribe 'G' = 'C'
    transcribe 'C' = 'G'
    transcribe 'T' = 'A'
    transcribe 'A' = 'U'
