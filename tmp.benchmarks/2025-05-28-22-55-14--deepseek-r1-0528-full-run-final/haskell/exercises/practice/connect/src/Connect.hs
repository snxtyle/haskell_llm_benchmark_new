module Connect (Mark(..), winner) where

import Data.Array.ST (newArray, readArray, writeArray, STUArray)
import Control.Monad (forM_, filterM)
import Control.Monad.ST (runST, ST)

data Mark = Cross | Nought deriving (Eq, Show)

winner :: [String] -> Maybe Mark
winner board 
  | hasOPath board = Just Nought
  | hasXPath board = Just Cross
  | otherwise = Nothing
  where 
    n = length board
    m = if n == 0 then 0 else length (head board)

hasOPath :: [String] -> Bool
hasOPath board = 
  let n = length board
      m = if n==0 then 0 else length (head board)
  in if n == 0 then False
     else 
         let starts = [ (0, j) | j <- [0..m-1], charAt (0,j) board == 'O' ]
         in bfs board 'O' starts (\(r,_) -> r == n-1) n m

hasXPath :: [String] -> Bool
hasXPath board = 
  let n = length board
      m = if n==0 then 0 else length (head board)
  in if m == 0 then False
     else 
         let starts = [ (i, 0) | i <- [0..n-1], charAt (i,0) board == 'X' ]
         in bfs board 'X' starts (\(_,c) -> c == m-1) n m

bfs :: [String] -> Char -> [Position] -> (Position -> Bool) -> Int -> Int -> Bool
bfs board player starts goal n m = runST $ do
  visited <- newArray ((0,0), (n-1, m-1)) False :: ST s (STUArray s (Int,Int) Bool)
  let queue = starts
  markVisited visited queue
  loop visited queue
  where
    markVisited :: STUArray s (Int,Int) Bool -> [Position] -> ST s ()
    markVisited vis ps = forM_ ps $ \p -> writeArray vis p True

    loop :: STUArray s (Int,Int) Bool -> [Position] -> ST s Bool
    loop vis [] = return False
    loop vis queue = do
      if any goal queue
        then return True
        else do
          let allNeighbors = [ (r', c') | (r,c) <- queue, (dr,dc) <- directions, 
                       let r' = r+dr; c' = c+dc,
                       inBounds r' c' n m,
                       charAt (r',c') board == player ]
          newNeighbors <- filterM (\p -> do
                                     v <- readArray vis p
                                     return (not v)) allNeighbors
          markVisited vis newNeighbors
          loop vis newNeighbors

directions :: [(Int, Int)]
directions = [ (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0) ]

type Position = (Int, Int)

inBounds :: Int -> Int -> Int -> Int -> Bool
inBounds r c n m = r >=0 && r < n && c>=0 && c<m

charAt :: (Int, Int) -> [String] -> Char
charAt (r,c) board = (board !! r) !! c
