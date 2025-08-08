module Spiral (spiral) where

spiral :: Int -> [[Int]]
spiral 0 = []
spiral size = generateSpiral size
  where
    generateSpiral :: Int -> [[Int]]
    generateSpiral n = 
      let matrix = replicate n (replicate n 0)
          filled = fillSpiral matrix 0 0 (n-1) (n-1) 1
      in filled
    
    fillSpiral :: [[Int]] -> Int -> Int -> Int -> Int -> Int -> [[Int]]
    fillSpiral matrix top left bottom right num
      | top > bottom || left > right = matrix
      | otherwise = 
          let -- Fill top row (left to right)
              matrix1 = foldl (\m col -> updateMatrix m top col (num + col - left)) 
                              matrix [left..right]
              num1 = num + (right - left + 1)
              
              -- Fill right column (top+1 to bottom)
              matrix2 = if top < bottom
                        then foldl (\m row -> updateMatrix m row right (num1 + row - top - 1)) 
                                   matrix1 [(top+1)..bottom]
                        else matrix1
              num2 = if top < bottom 
                     then num1 + (bottom - top)
                     else num1
              
              -- Fill bottom row (right-1 to left)
              matrix3 = if top < bottom && left < right
                        then foldl (\m col -> updateMatrix m bottom col (num2 + (right - 1) - col)) 
                                   matrix2 (reverse [left..(right-1)])
                        else matrix2
              num3 = if top < bottom && left < right
                     then num2 + (right - left)
                     else num2
              
              -- Fill left column (bottom-1 to top+1)
              matrix4 = if top < bottom - 1 && left < right
                        then foldl (\m row -> updateMatrix m row left (num3 + (bottom - 1) - row)) 
                                   matrix3 (reverse [(top+1)..(bottom-1)])
                        else matrix3
              num4 = if top < bottom - 1 && left < right
                     then num3 + (bottom - top - 1)
                     else num3
          
          in fillSpiral matrix4 (top+1) (left+1) (bottom-1) (right-1) num4
    
    updateMatrix :: [[Int]] -> Int -> Int -> Int -> [[Int]]
    updateMatrix matrix row col val = 
      take row matrix ++ 
      [take col (matrix !! row) ++ [val] ++ drop (col + 1) (matrix !! row)] ++
      drop (row + 1) matrix
