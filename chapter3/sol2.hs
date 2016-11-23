bsearch :: (Int -> Int) -> (Int, Int) -> Int -> Int
bsearch g (z1, z2) gz
  | z1 == z2 - 1  = z2
  | g m > gz      = bsearch g (z1, m) gz
  | otherwise     = bsearch g (m, z2) gz
    where
      m = (z1 + z2) `div` 2

find :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
find f z = findFrom f z (xstart, 0) yend
  where
    xstart  = bsearch (flip f $ 0) (0, z) z
    yend    = bsearch (f 0) (0, z) z

findFrom :: (Int -> Int -> Int) -> Int -> (Int, Int) -> Int -> [(Int, Int)]
findFrom f z pt@(x, y) yend
  | x < 0 || y > yend = []
  | fxy == z          = pt:(findFrom f z (x - 1, y + 1) yend)
  | fxy > z           = findFrom f z (x - 1, y) yend
  | fxy < z           = findFrom f z (x, y + 1) yend
    where
      fxy = uncurry f pt
