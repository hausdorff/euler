-- utility functions
factorial 0 = 1
factorial n = product [1..n]

nCr n r = (factorial n) `div` (factorial r * factorial (n - r))


-- explicitly calculates via recursion
paths 0 0 = 1
paths 0 y = paths 0 (y - 1)
paths x 0 = paths (x - 1) 0
paths x y = (paths (x - 1) y) + (paths x (y - 1))

-- calculate paths via combinatorics
-- We are allowed to move only down or right. For an nxn matrix, there will be a
-- total of n moves down and n moves right, or 2n moves total. The standard
-- binomial coefficient tells us that there are `nCr (2*n) n` ways to pick
-- a sequence of 2n with n moves right (or down, it doesn't matter). Thus the PE
-- solutions is, a 20 x 20 grid should have 137846528820 paths.
paths' n = (2*n) `nCr` n

main =
  print $ paths' 20