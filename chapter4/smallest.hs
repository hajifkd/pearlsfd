import Data.Array

smallest :: Ord a => Int -> ([a], [a]) -> a
smallest k ([], ys) = ys !! k
smallest k (xs, []) = xs !! k
smallest k (xs, ys) =
  case (k < lhx + lhy + 1, p < q) of
    (True, True)    -> smallest k (xs, fys)
    (False, True)   -> smallest (k - lhx - 1) (sxs, ys)
    (True, False)   -> smallest k (ys, fxs)
    (False, False)  -> smallest (k - lhy - 1) (sys, xs)
  where
    lhx = length xs `div` 2
    lhy = length ys `div` 2
    (fxs, p:sxs) = splitAt lhx xs
    (fys, q:sys) = splitAt lhy ys


smallestA :: Ord a => Int -> (Array Int a, Array Int a) -> a
smallestA k (xa, ya) = search k (0, xmax + 1) (0, ymax + 1)
  where
    (0, xmax) = bounds xa
    (0, ymax) = bounds ya
    search k (sx, fx) (sy, fy)
      | sx == fx    = ya ! (k + sy)
      | sy == fy    = xa ! (k + sx)
      | otherwise   = case (k < lhx + lhy + 1, xa ! (sx + lhx) < ya ! (sy + lhy)) of
                        (True, True)    -> search k (sx, fx) (sy, sy + lhy)
                        (False, True)   -> search (k - lhx - 1) (sx + lhx + 1, fx) (sy, fy)
                        (True, False)   -> search k (sx, sx + lhx) (sy, fy)
                        (False, False)  -> search (k - lhy - 1) (sx, fx) (sy + lhy + 1, fy)
                      where
                        lhx = (fx - sx) `div` 2
                        lhy = (fy - sy) `div` 2
