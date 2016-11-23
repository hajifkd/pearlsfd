find :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
find f z = findFrom f z (z, 0)

findFrom :: (Int -> Int -> Int) -> Int -> (Int, Int) -> [(Int, Int)]
findFrom f z pt@(x, y)
  | x < 0 || y > z  = []
  | fxy == z        = pt:(findFrom f z (x - 1, y + 1))
  | fxy > z         = findFrom f z (x - 1, y)
  | fxy < z         = findFrom f z (x, y + 1)
    where
      fxy = uncurry f pt
