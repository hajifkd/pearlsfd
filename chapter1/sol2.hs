import Data.Array

minfree :: [Int] -> Int
minfree = findFalse . buildList

findFalse :: Array Int Bool -> Int
findFalse = length . takeWhile id . elems

buildList :: [Int] -> Array Int Bool
buildList xs = accumArray (||) False (0, n) $
                 zip [x | x <- xs, x <= n] $ repeat True
               where
                 n = length xs
