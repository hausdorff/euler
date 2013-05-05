-- Since 1 must be at the center, all such squares must have an odd number of
-- rows. Now, consider this example:
--
-- 21 22 23 24 25
-- 20  7  8  9 10
-- 19  6  1  2 11
-- 18  5  4  3 12
-- 17 16 15 14 13
--
-- If we solve the 3x3 case first, we see that the total in the diagonals is
-- 1+3+5+7+9=24. Now, the next number in the new 5x5 matrix after the last
-- diagonal (that is, 9) is 10. We notice that if we count n-1=4 downwards,
-- we get to 13, which is the next diagonal. Then we count n-1=4 spaces left
-- and end up at the next diagonal. And then up n-1=4 spaces, and then right
-- n-1=4 spaces.
--
-- This function calculates sums of nxn grids by doing basically this same
-- strategy. It counts down n-1 times, then left, then up, then right. It
-- accumulates these sums as the first argument of the tuple, and keeps the
-- last diagonal from the previous solution as the second argument of the
-- tuple.
--
-- Since it doesn't make sense to look at squares smaller than 3x3, our base
-- case is 3x3.
_sqSum' :: Int -> (Int, Int)
_sqSum' 2 = (1 + 3 + 5 + 7 + 9, 9)
_sqSum' n = (currtotal + i + i' + i'' + i''', diag')
  where (currtotal, diag) = _sqSum' (n-2)
        i                 = diag + n
        i'                = i + n
        i''               = i' + n
        i'''              = i'' + n
        diag'             = diag + 4*n

sqSum :: Int -> (Int, Int)
sqSum n | odd n     = _sqSum' (n-1)
        | otherwise = error "the NxN square always have an odd number of sides"

-- 669171001
solution :: Int
solution = fst $ sqSum 1001

-- EDIT: a friend's solution is much more elegant. I am humbled.
solution' = sum (map (\n -> 4*(n-2)^2+10*(n-1)) [3,5..1001]) + 1

main :: IO ()
main = print solution
