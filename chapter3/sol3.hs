import Debug.Trace

bsearch :: (Int -> Int) -> (Int, Int) -> Int -> Int
bsearch g (z1, z2) gz
  | z1 >= z2 - 1  = z2
  | g m >= gz     = bsearch g (z1, m) gz
  | otherwise     = bsearch g (m, z2) gz
    where
      m = (z1 + z2) `div` 2

find :: (Int -> Int -> Int) -> Int -> [(Int, Int)]
find f z = findRec f (0, ystart) (xend, 0) z
  where
    xend    = bsearch (flip f $ 0) (0, z) z
    ystart  = bsearch (f 0) (0, z) z

findRec :: (Int -> Int -> Int) -> (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
findRec f leftup@(lx, ly) rightdown@(rx, ry) z
  | lx < 0 || ly > z || rx > z || ry < 0  = []
  | my == ly                              = if eqn then [(mx, my)] else []
  | eqn                                   = (mx, my):rest
  | otherwise                             = rest
    where
      mx    = (lx + rx) `div` 2
      my    = bsearch (f mx) (ry, ly) z
      eqn   = f mx my == z
      mid1  = (mx - 1, if eqn then my + 1 else my)
      mid2  = (if eqn then mx + 1 else mx, my - 1)
      rest  = (findRec f leftup mid1 z) ++ (findRec f mid2 rightdown z)
