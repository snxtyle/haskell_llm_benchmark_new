module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse toRNABase
  where
    toRNABase 'G' = Right 'C'
    toRNABase 'C' = Right 'G'
    toRNABase 'T' = Right 'A'
    toRNABase 'A' = Right 'U'
    toRNABase c   = Left c
