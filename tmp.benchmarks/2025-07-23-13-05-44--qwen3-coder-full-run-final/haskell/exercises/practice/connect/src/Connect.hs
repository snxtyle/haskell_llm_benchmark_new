module Connect (Mark(..), winner) where

import Data.Array
import qualified Data.Set as Set

data Mark = Cross | Nought deriving (Eq, Show)

type Board = Array (Int, Int) (Maybe Mark)
type Position = (Int, Int)
type Visited = Set.Set Position

winner :: [String] -> Maybe Mark
winner [] = Nothing
winner [""] = Nothing
winner boardLines = 
  let board = parseBoard boardLines
      bounds = arrayBounds board
      (maxRow, maxCol) = snd bounds
  in case (hasWon Cross board bounds maxRow maxCol, hasWon Nought board bounds maxRow maxCol) of
       (True, _) -> Just Cross
       (_, True) -> Just Nought
       _ -> Nothing

parseBoard :: [String] -> Board
parseBoard boardLines = 
  let rows = filter (not . null) boardLines
      maxRow = if null rows then -1 else length rows - 1
      maxCol = if null rows then -1 else maximum (map length rows) - 1
      bounds = ((0, 0), (maxRow, maxCol))
      cells = if null rows 
              then []
              else [((r, c), charToMark (getRowChar rows r c)) | r <- [0..maxRow], c <- [0..(length (rows !! r) - 1)]]
  in if null rows 
     then array ((0,0),((-1,-1))) []
     else array bounds cells

charToMark :: Char -> Maybe Mark
charToMark 'X' = Just Cross
charToMark 'O' = Just Nought
charToMark _ = Nothing

getRowChar :: [String] -> Int -> Int -> Char
getRowChar rows r c = 
  let row = rows !! r
  in if c < length row then row !! c else '.'

arrayBounds :: Board -> ((Int, Int), (Int, Int))
arrayBounds = bounds

hasWon :: Mark -> Board -> ((Int, Int), (Int, Int)) -> Int -> Int -> Bool
hasWon mark board bounds@((minRow, minCol), (maxRow, maxCol)) maxRow' maxCol' = 
  if minRow > maxRow || minCol > maxCol
  then False
  else case mark of
    Cross ->  -- X connects left to right columns
      let startingPositions = [(r, 0) | r <- [minRow..maxRow], inBounds bounds (r, 0) && board ! (r, 0) == Just Cross]
      in any (\start -> canReachSide Cross board bounds start (1, 0) (Set.singleton start)) startingPositions
    Nought -> -- O connects top to bottom rows
      let startingPositions = [(0, c) | c <- [minCol..maxCol], inBounds bounds (0, c) && board ! (0, c) == Just Nought]
      in any (\start -> canReachSide Nought board bounds start (0, 1) (Set.singleton start)) startingPositions

canReachSide :: Mark -> Board -> ((Int, Int), (Int, Int)) -> Position -> (Int, Int) -> Visited -> Bool
canReachSide mark board bounds current@(r, c) direction visited =
  let (_, (maxRow, maxCol)) = bounds
  in case direction of
       (1, 0) -> c == maxCol  -- For X, check if we reached the rightmost column
       (0, 1) -> r == maxRow  -- For O, check if we reached the bottom row
       _ -> False
  || any (\next -> 
           inBounds bounds next && 
           board ! next == Just mark && 
           not (Set.member next visited) && 
           canReachSide mark board bounds next direction (Set.insert next visited))
         (neighbors current)

inBounds :: ((Int, Int), (Int, Int)) -> Position -> Bool
inBounds ((minR, minC), (maxR, maxC)) (r, c) = 
  r >= minR && r <= maxR && c >= minC && c <= maxC

neighbors :: Position -> [Position]
neighbors (r, c) = 
  [(r-1, c), (r+1, c), (r, c-1), (r, c+1), (r-1, c+1), (r+1, c-1)]
