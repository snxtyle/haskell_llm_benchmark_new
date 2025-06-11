module GameOfLife (tick) where

tick :: [[Int]] -> [[Int]]
tick [] = []
tick grid@(g:_) =
  let
    height = length grid
    width = length g

    -- Safely get cell value, treating out-of-bounds as dead (0).
    -- It closes over `grid`, `height`, and `width` from the parent scope.
    cellValue y x
      | y < 0 || y >= height || x < 0 || x >= width = 0
      | otherwise = grid !! y !! x

    -- Count live neighbors for a cell at (y, x) by summing the values
    -- of all 8 adjacent cells.
    countLiveNeighbors y x =
      sum [cellValue (y+dy) (x+dx) | dy <- [-1..1], dx <- [-1..1], (dy, dx) /= (0,0)]

    -- Apply Game of Life rules based on current state and live neighbor count.
    evolve 1 n | n == 2 || n == 3 = 1 -- A live cell with 2 or 3 neighbors survives.
    evolve 0 3                    = 1 -- A dead cell with 3 neighbors becomes alive.
    evolve _ _                    = 0 -- All other cells die or stay dead.

    -- Calculate the next state for a cell at (y, x).
    nextState y x = evolve (cellValue y x) (countLiveNeighbors y x)

  in
    -- Build the next generation grid using a list comprehension.
    [[nextState y x | x <- [0..width-1]] | y <- [0..height-1]]
