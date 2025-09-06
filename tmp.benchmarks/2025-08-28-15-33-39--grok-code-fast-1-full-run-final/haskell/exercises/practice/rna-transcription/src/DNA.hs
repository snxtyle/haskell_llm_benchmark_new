module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = traverse mapNuc xs
  where
    mapNuc :: Char -> Either Char Char
    mapNuc 'G' = Right 'C'
    mapNuc 'C' = Right 'G'
    mapNuc 'T' = Right 'A'
    mapNuc 'A' = Right 'U'
    mapNuc c   = Left c
