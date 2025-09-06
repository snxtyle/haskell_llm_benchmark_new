module TwoBucket (measure) where

measure :: (Int, Int) -> Int -> Maybe (Int, (Int, Int))
measure (cap1, cap2) target
  | target > max cap1 cap2 || target < 0 = Nothing
  | otherwise = bfs [((cap1, 0), 1)] [(cap1, 0)]
  where
    bfs [] _ = Nothing
    bfs ((state@(a, b), steps):rest) visited
      | a == target || b == target = Just (steps, state)
      | otherwise = let
          neigh = filter (\s -> s `notElem` visited && valid s) (neighbors cap1 cap2 state)
          newQueue = rest ++ map (\s -> (s, steps + 1)) neigh
          newVisited = visited ++ neigh
        in bfs newQueue newVisited

    valid (a', b') = not (a' == 0 && b' == cap2)

    neighbors cap1 cap2 (a, b) = concat [
        if a < cap1 then [(cap1, b)] else [],
        if b < cap2 then [(a, cap2)] else [],
        if a > 0 then [(0, b)] else [],
        if b > 0 then [(a, 0)] else [],
        if a > 0 && b < cap2 then let p = min a (cap2 - b) in [(a - p, b + p)] else [],
        if b > 0 && a < cap1 then let p = min b (cap1 - a) in [(a + p, b - p)] else []
      ]
