module WordSearch (search, CharPos(..), WordPos(..)) where

data CharPos = CharPos{col::Int, row::Int} deriving (Eq, Show)
data WordPos = WordPos{start::CharPos, end::CharPos} deriving (Eq, Show)

search :: [String] -> [String] -> [(String, Maybe WordPos)]
search grid wordList = map (\word -> (word, findWord grid word)) wordList

findWord :: [String] -> String -> Maybe WordPos
findWord grid word
  | null word = Nothing  -- Handle empty words
  | otherwise =
      let rows = length grid
          cols = if null grid then 0 else length (head grid)
          len = length word
          dirs = [(1,0), (-1,0), (0,1), (0,-1), (1,1), (-1,1), (1,-1), (-1,-1)]
          candidates = [ WordPos (CharPos c r) (CharPos (c + (len-1)*dx) (r + (len-1)*dy)) |
                         r <- [0..rows-1],
                         c <- [0..cols-1],
                         (dx,dy) <- dirs,
                         let positions = [(r + i*dy, c + i*dx) | i <- [0..len-1]],
                         all (\(rr,cc,i) -> rr >= 0 && rr < rows && cc >= 0 && cc < cols && grid !! rr !! cc == word !! i)
                             [(rr,cc,i) | ((rr,cc),i) <- zip positions [0..]] ]
      in case candidates of
           [] -> Nothing
           (wp:_) -> Just wp
