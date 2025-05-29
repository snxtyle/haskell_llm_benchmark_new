module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import qualified Data.Set as S

newtype School = School (M.Map Int (S.Set String))

add :: Int -> String -> School -> School
add gradeNum student (School m) = 
    School $ M.alter (Just . maybe (S.singleton student) (S.insert student)) gradeNum m

empty :: School
empty = School M.empty

grade :: Int -> School -> [String]
grade gradeNum (School m) = 
    case M.lookup gradeNum m of
        Nothing -> []
        Just s  -> S.toList s

sorted :: School -> [(Int, [String])]
sorted (School m) = 
    [(g, S.toList s) | (g, s) <- M.toList m]
