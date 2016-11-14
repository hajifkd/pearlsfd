join :: Ord a => Int -> [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join _ [] tys = tys
join _ txs [] = txs
join ly txs@((x, c):txs') tys@((y, c'):tys')
  | x < y     = (x, c + ly):join ly txs' tys
  | otherwise = (y, c'):join (ly - 1) txs tys'

table :: Ord a => [a] -> [(a, Int)]
table [x] = [(x, 0)]
table xs = join lz (table ys) (table zs)
           where
             m = length xs
             n = m `div` 2
             (ys, zs) = splitAt n xs
             lz = m - n

msc :: Ord a => [a] -> Int
msc = maximum . map snd . table

