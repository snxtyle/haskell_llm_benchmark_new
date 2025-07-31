module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA (x:xs) = case x of
    'G' -> case toRNA xs of
        Right rest -> Right ('C':rest)
        Left err -> Left err
    'C' -> case toRNA xs of
        Right rest -> Right ('G':rest)
        Left err -> Left err
    'T' -> case toRNA xs of
        Right rest -> Right ('A':rest)
        Left err -> Left err
    'A' -> case toRNA xs of
        Right rest -> Right ('U':rest)
        Left err -> Left err
    _ -> Left x
