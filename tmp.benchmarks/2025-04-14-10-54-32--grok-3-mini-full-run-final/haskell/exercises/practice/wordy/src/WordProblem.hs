module WordProblem (answer) where

data Operation = Add | Subtract | Multiply | Divide

answer :: String -> Maybe Integer
answer problem = case words problem of
    ("What":"is":rest) -> evaluate rest
    _ -> Nothing
  where
    evaluate :: [String] -> Maybe Integer
    evaluate [] = Nothing
    evaluate (first:rest) = case reads first of
        [(num, restStr)] | restStr == "?" -> applyOperations num rest
        _ -> Nothing

    applyOperations :: Integer -> [String] -> Maybe Integer
    applyOperations acc xs
        | null xs || xs == ["?"] = Just acc
        | otherwise = case getOperation xs of
            Just (op, ys) -> case getNumber ys of
                Just (num, zs) -> case apply op acc num of
                    Just newAcc -> applyOperations newAcc zs
                    Nothing -> Nothing
                Nothing -> Nothing
            Nothing -> Nothing  -- Changed to Nothing for safety

    getOperation :: [String] -> Maybe (Operation, [String])
    getOperation ("plus":rest) = Just (Add, rest)
    getOperation ("minus":rest) = Just (Subtract, rest)
    getOperation ("multiplied":"by":rest) = Just (Multiply, rest)
    getOperation ("divided":"by":rest) = Just (Divide, rest)
    getOperation _ = Nothing

    getNumber :: [String] -> Maybe (Integer, [String])
    getNumber (first:rest) = case reads first of
        [(num, restStr)] | restStr == "?" -> Just (num, rest)
        _ -> Nothing
    getNumber [] = Nothing

    apply :: Operation -> Integer -> Integer -> Maybe Integer
    apply Add a b = Just (a + b)
    apply Subtract a b = Just (a - b)
    apply Multiply a b = Just (a * b)
    apply Divide a b
        | b /= 0 && a `mod` b == 0 = Just (a `div` b)
        | otherwise = Nothing
