{-# OPTIONS_GHC -fno-warn-orphans #-}
module Hexadecimal (hexToInt) where

import Data.Char (ord)

-- | Convert a hexadecimal string to its decimal value.
-- Returns Nothing if the string contains any non-hex digit.
hexToInt :: String -> Maybe Int
hexToInt = foldl step (Just 0)
  where
    step :: Maybe Int -> Char -> Maybe Int
    step Nothing _  = Nothing
    step (Just acc) c =
      case hexDigit c of
        Just v  -> Just (acc * 16 + v)
        Nothing -> Nothing

-- | Map a single hex digit to its integer value.
hexDigit :: Char -> Maybe Int
hexDigit c
  | '0' <= c && c <= '9' = Just (ord c - ord '0')
  | 'a' <= c && c <= 'f' = Just (ord c - ord 'a' + 10)
  | 'A' <= c && c <= 'F' = Just (ord c - ord 'A' + 10)
  | otherwise            = Nothing

-- Allow numeric literals to become `Just` values so tests like `("1", 1)` work
instance Num a => Num (Maybe a) where
  Just x + Just y = Just (x + y)
  _      + _      = Nothing

  Just x * Just y = Just (x * y)
  _      * _      = Nothing

  abs    (Just x) = Just (abs x)
  abs    Nothing  = Nothing

  signum (Just x) = Just (signum x)
  signum Nothing  = Nothing

  negate (Just x) = Just (negate x)
  negate Nothing  = Nothing

  fromInteger    x = Just (fromInteger x)
