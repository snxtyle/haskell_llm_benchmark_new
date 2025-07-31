module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = go []
  where
    go acc [] = Right (reverse acc)
    go acc (x:xs) =
      case transcribe x of
        Just r  -> go (r:acc) xs
        Nothing -> Left x

    transcribe :: Char -> Maybe Char
    transcribe 'G' = Just 'C'
    transcribe 'C' = Just 'G'
    transcribe 'T' = Just 'A'
    transcribe 'A' = Just 'U'
    transcribe _   = Nothing
