module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0 = []
spiral size = 
  let -- Create initial empty grid
      emptyGrid = replicate size (replicate size 0)
      
      -- Fill the grid in spiral order
      fillSpiral grid row col num dr dc
        | num > size * size = grid
        | otherwise =
            let -- Update current cell
                grid' = updateGrid grid row col num
                
                -- Calculate next position
                nextRow = row + dr
                nextCol = col + dc
                
                -- Check if we need to turn
                needTurn = nextRow < 0 || nextRow >= size || 
                          nextCol < 0 || nextCol >= size ||
                          (getCell grid' nextRow nextCol /= 0)
            in
            if needTurn
              then 
                -- Turn clockwise: right->down, down->left, left->up, up->right
                let (newDr, newDc) = (dc, -dr)
                in fillSpiral grid' (row + newDr) (col + newDc) (num + 1) newDr newDc
              else
                fillSpiral grid' nextRow nextCol (num + 1) dr dc
      
      -- Helper to get cell value
      getCell grid row col = (grid !! row) !! col
      
      -- Helper to update cell value
      updateGrid grid row col val =
        let (before, rowList:after) = splitAt row grid
            (beforeCol, _:afterCol) = splitAt col rowList
            newRow = beforeCol ++ [val] ++ afterCol
        in before ++ [newRow] ++ after
  in
  -- Start at top-left (0,0), moving right (dr=0, dc=1)
  fillSpiral emptyGrid 0 0 1 0 1
