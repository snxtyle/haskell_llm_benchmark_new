module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = transcribe []
  where
    transcribe :: String -> String -> Either Char String
    transcribe acc [] = Right (reverse acc)
    transcribe acc (x:xs) = case complement x of
      Just c  -> transcribe (c:acc) xs
      Nothing -> Left x
    
    complement :: Char -> Maybe Char
    complement 'G' = Just 'C'
    complement 'C' = Just 'G'
    complement 'T' = Just 'A'
    complement 'A' = Just 'U'
    complement _   = Nothing
