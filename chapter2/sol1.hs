msc :: Ord a => [a] -> Int
msc xs = maximum $ scount xs

scount :: Ord a => [a] -> [Int]
scount []     = []
scount (x:xs) = length [1 | c <- xs, x < c] : scount xs
