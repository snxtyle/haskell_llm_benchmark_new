module Change (findFewestCoins) where

import Data.List (sort)

findFewestCoins :: Integer -> [Integer] -> Maybe [Integer]
findFewestCoins target coins
  | target < 0 = Nothing
  | target == 0 = Just []
  | null coins = Nothing
  | otherwise =
      let posCoins = filter (> 0) coins
          -- Work with positive coins only
          -- Dynamic programming arrays up to target
          -- bestCount[i] = minimal number of coins to make i, or inf (Nothing)
          -- lastCoin[i] = coin chosen last to achieve bestCount[i]
          inf = maxBound :: Int
          t = fromInteger target :: Int
          -- Build arrays iteratively
          (bestCount, lastCoin) = dp t (replicate (t + 1) inf) (replicate (t + 1) Nothing)
          dp 0 bc lc = (replace 0 0 bc, replace 0 Nothing lc)
          dp n bc lc =
            let (bcPrev, lcPrev) = dp (n - 1) bc lc
                -- try all coins to update for amount n
                choices = [ (1 + bcPrev !! (n - fromInteger c), Just c)
                          | c <- posCoins
                          , fromInteger c <= n
                          , bcPrev !! (n - fromInteger c) /= inf
                          ]
                (bestN, lastN) =
                  case choices of
                    [] -> (bcPrev !! n, lcPrev !! n)
                    _  ->
                      -- pick minimal count; if tie, prefer smaller resulting coin sequence by choosing smaller coin
                      -- Using sort on coins and picking minimal count with smallest coin is sufficient
                      let minCount = minimum (map fst choices)
                          candidateCoins = [ c | (cnt, Just c) <- choices, cnt == minCount ]
                          chosenCoin = Just (minimum candidateCoins)
                      in (minCount, chosenCoin)
            in (replace n bestN bcPrev, replace n lastN lcPrev)
          replace idx val xs =
            let (a, _ : b) = splitAt idx xs in a ++ val : b
      in
        if null posCoins
          then Nothing
          else
            let (bcFinal, lcFinal) = (bestCount, lastCoin)
                best = bcFinal !! t
            in if best == (maxBound :: Int)
                 then Nothing
                 else Just (reconstruct t lcFinal)
  where
    reconstruct 0 _ = []
    reconstruct amt lc =
      case lc !! amt of
        Nothing -> []
        Just c ->
          let nextAmt = amt - fromInteger c
          in sort (c : reconstruct nextAmt lc)
