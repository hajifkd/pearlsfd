import Data.List

minfree :: [Int] -> Int
minfree xs = findhead 0 (length xs, xs)

findhead :: Int -> (Int, [Int]) -> Int
findhead s (n, xs) | n == 0       = s
                   | c == offset  = findhead b (n - c, vs)
                   | otherwise    = findhead s (c, us)
                     where
                       (us, vs) = partition (< b) xs
                       offset = 1 + n `div` 2
                       b = s + offset
                       c = length us

