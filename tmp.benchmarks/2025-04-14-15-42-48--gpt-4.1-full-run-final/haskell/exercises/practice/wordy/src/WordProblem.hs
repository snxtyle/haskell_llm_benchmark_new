module WordProblem (answer) where

import Data.Char (isDigit, isSpace)
import Data.List (stripPrefix)
import Text.Read (readMaybe)

data Op = Add | Subtract | Multiply | Divide deriving (Eq, Show)

-- Parse a single operation word into Op
parseOp :: String -> Maybe Op
parseOp "plus" = Just Add
parseOp "minus" = Just Subtract
parseOp "multiplied" = Just Multiply
parseOp "divided" = Just Divide
parseOp _ = Nothing

-- Parse a token, which may be a number or an operation
parseToken :: String -> Maybe (Either Integer Op)
parseToken s =
    case parseOp s of
        Just op -> Just (Right op)
        Nothing -> case readMaybe s :: Maybe Integer of
            Just n -> Just (Left n)
            Nothing -> Nothing

-- Tokenize the problem string into numbers and operations
tokenize :: String -> Maybe [Either Integer Op]
tokenize s =
    let s' = filter (/= '?') s
        ws = words s'
        -- Remove "What is" prefix
        rest = dropWhile isSpace $ dropPrefix "What is " s'
    in parseTokens (words rest)
  where
    dropPrefix pfx str = maybe str id (stripPrefix pfx str)
    parseTokens [] = Nothing
    parseTokens ws = go ws []
      where
        go [] acc = Just (reverse acc)
        go ("plus":xs) acc = go xs (Right Add : acc)
        go ("minus":xs) acc = go xs (Right Subtract : acc)
        go ("multiplied":"by":xs) acc = go xs (Right Multiply : acc)
        go ("divided":"by":xs) acc = go xs (Right Divide : acc)
        go (x:xs) acc =
            case readMaybe x :: Maybe Integer of
                Just n -> go xs (Left n : acc)
                Nothing -> Nothing

-- Evaluate the list of tokens left-to-right
evalTokens :: [Either Integer Op] -> Maybe Integer
evalTokens [] = Nothing
evalTokens (Left n : rest) = go n rest
  where
    go acc [] = Just acc
    go acc (Right op : Left n2 : xs) =
        let acc' = case op of
                Add -> acc + n2
                Subtract -> acc - n2
                Multiply -> acc * n2
                Divide -> if n2 == 0 then acc else acc `div` n2
        in go acc' xs
    go _ _ = Nothing
evalTokens _ = Nothing

-- Main answer function
answer :: String -> Maybe Integer
answer s =
    case tokenize s of
        Just tokens -> evalTokens tokens
        Nothing -> Nothing
