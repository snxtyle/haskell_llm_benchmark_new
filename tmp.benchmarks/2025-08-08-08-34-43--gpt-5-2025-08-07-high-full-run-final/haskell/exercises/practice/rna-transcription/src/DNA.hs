module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = go
  where
    go [] = Right []
    go (x:xs) =
      case transcribe x of
        Left bad -> Left bad
        Right y ->
          case go xs of
            Left bad -> Left bad
            Right ys -> Right (y : ys)

    transcribe :: Char -> Either Char Char
    transcribe 'G' = Right 'C'
    transcribe 'C' = Right 'G'
    transcribe 'T' = Right 'A'
    transcribe 'A' = Right 'U'
    transcribe c   = Left c
