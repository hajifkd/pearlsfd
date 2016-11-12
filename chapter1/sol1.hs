minfree :: [Int] -> Int
minfree xs = head $ [0..] // xs

(//) :: Eq a => [a] -> [a] -> [a]
--xs // ys = [x | x <- xs, foldr (\y r -> r && y /= x) True ys]
--xs // ys = [x | x <- xs, notElem x ys]
--xs // ys = [x | x <- xs, all (/= x) ys]
xs // ys = [x | x <- xs, foldr ((&&).(/= x)) True ys]
