module Spiral (spiral) where

import Control.Monad.ST (ST)
import Data.Array.ST (STUArray, runSTUArray)
import Data.Array.MArray (newArray, writeArray)
import Data.Array.Unboxed (UArray, (!))

spiral :: Int -> [[Int]]
spiral 0 = []
spiral n =
    -- Define the bounds for the 2D array (0-indexed)
    let bounds = ((0, 0), (n - 1, n - 1))
        
        -- This ST action creates and fills the mutable array
        fillArray :: ST s (STUArray s (Int, Int) Int)
        fillArray = do
            -- Create a new mutable unboxed array, initialized with a dummy value (e.g., 0)
            marr <- newArray bounds 0 :: ST s (STUArray s (Int, Int) Int)
            
            -- Initial parameters for the spiral generation loop
            let initialNum = 1
            let initialR = 0
            let initialC = 0
            let initialDir = 0 -- 0: Right, 1: Down, 2: Left, 3: Up
            
            -- Boundaries of the current layer of the spiral
            let initialMinR = 0
            let initialMaxR = n - 1
            let initialMinC = 0
            let initialMaxC = n - 1
            
            -- Recursive loop function to fill the array
            -- (num, r, c, dir, minR, maxR, minC, maxC)
            let loop num r c dir minR maxR minC maxC = do
                    if num > n * n then return () -- All cells filled
                    else do
                        writeArray marr (r, c) num -- Fill current cell
                        
                        case dir of
                            0 -> -- Moving Right
                                if c == maxC then -- Hit right boundary
                                    -- Turn Down; next cell is (r+1, c)
                                    -- Shrink top boundary (minR increases)
                                    loop (num + 1) (r + 1) c 1 (minR + 1) maxR minC maxC
                                else
                                    -- Continue Right; next cell is (r, c+1)
                                    loop (num + 1) r (c + 1) 0 minR maxR minC maxC
                            
                            1 -> -- Moving Down
                                if r == maxR then -- Hit bottom boundary
                                    -- Turn Left; next cell is (r, c-1)
                                    -- Shrink right boundary (maxC decreases)
                                    loop (num + 1) r (c - 1) 2 minR maxR minC (maxC - 1)
                                else
                                    -- Continue Down; next cell is (r+1, c)
                                    loop (num + 1) (r + 1) c 1 minR maxR minC maxC

                            2 -> -- Moving Left
                                if c == minC then -- Hit left boundary
                                    -- Turn Up; next cell is (r-1, c)
                                    -- Shrink bottom boundary (maxR decreases)
                                    loop (num + 1) (r - 1) c 3 minR (maxR - 1) minC maxC
                                else
                                    -- Continue Left; next cell is (r, c-1)
                                    loop (num + 1) r (c - 1) 2 minR maxR minC maxC

                            3 -> -- Moving Up
                                if r == minR then -- Hit top boundary (which has been shrunk)
                                    -- Turn Right; next cell is (r, c+1)
                                    -- Shrink left boundary (minC increases)
                                    loop (num + 1) r (c + 1) 0 minR maxR (minC + 1) maxC
                                else
                                    -- Continue Up; next cell is (r-1, c)
                                    loop (num + 1) (r - 1) c 3 minR maxR minC maxC
                            _ -> error "Invalid direction" -- Should not be reached

            -- Start the recursive loop
            loop initialNum initialR initialC initialDir initialMinR initialMaxR initialMinC initialMaxC
            
            return marr -- Return the filled mutable array

        -- Run the ST action to get an immutable UArray
        finalArray :: UArray (Int, Int) Int
        finalArray = runSTUArray fillArray
        
    -- Convert the UArray to the required [[Int]] format
    in [ [ finalArray ! (r, c) | c <- [0..n-1] ] | r <- [0..n-1] ]
