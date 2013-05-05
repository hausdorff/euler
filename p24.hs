import Data.List

-- Permutes the string containing digits, turns them into integers, sorts
-- those integers. To get the solution, we then return the millionth entry.
permutedNums :: [Integer]
permutedNums = sort $ map read $ permutations "0123456789"

-- slow solution
solution = permutedNums !! 999999

-- faster
permutations' :: [Char] -> [[Char]]
permutations' [] = [[]]
permutations' xs = [x:ys | x <- xs, ys <- permutations' (delete x xs)]
solution' :: Integer
solution' = (map read $ permutations' "0123456789") !! 999999