module Transpose (transpose) where

import Data.List (find, transpose)

transpose :: [String] -> [String]
transpose [] = []
transpose rows = trimmed
  where
    maxLength = maximum (map length rows)
    paddedRows = map (\s -> s ++ replicate (maxLength - length s) ' ') rows
    transposed = Data.List.transpose paddedRows
    originalLengths = map length rows
    reversedIndices = reverse [0 .. length rows - 1]

    getOutputLength colIndex =
      case find (\rowIndex -> (originalLengths !! rowIndex) > colIndex) reversedIndices of
        Just lastRowIndex -> lastRowIndex + 1
        Nothing -> 0

    trimmed = zipWith (\colIndex rowStr -> take (getOutputLength colIndex) rowStr) [0..] transposed
