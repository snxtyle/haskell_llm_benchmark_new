module Diamond (diamond) where

-- | Build a diamond of letters.
--   Returns `Nothing` if the supplied character is not an upper‑case
--   latin letter between @'A'@ and @'Z'@ (inclusive).
--
--   Examples (spaces shown as ·):
--
--   >>> diamond 'A'
--   Just ["A"]
--
--   >>> diamond 'C'
--   Just ["  A  "
--        ," B B "
--        ,"C   C"
--        ," B B "
--        ,"  A  "]
--
diamond :: Char -> Maybe [String]
diamond c
  | c < 'A' || c > 'Z' = Nothing
  | otherwise          = Just (buildDiamond c)

-- Internal helpers -----------------------------------------------------------

-- | Construct the full list of rows for a valid input character.
buildDiamond :: Char -> [String]
buildDiamond c = topRows ++ bottomRows
  where
    letters     = ['A' .. c]            -- ascending list from A to the target
    n           = length letters - 1    -- index of the last row (0‑based)

    -- Build the top half (including the middle row).
    topRows     = zipWith makeRow [0 ..] letters

    -- Bottom half is the mirror image of the top, excluding the middle row.
    bottomRows  = tail (reverse topRows)

    -- Create a single row given its index and letter.
    makeRow i l =
      let leading = replicate (n - i) ' '
          inner   = if i == 0
                      then ""
                      else replicate (2 * i - 1) ' '
      in leading
         ++ [l]
         ++ inner
         ++ (if i == 0 then "" else [l])
         ++ leading
