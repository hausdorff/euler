import Data.Set

toSet :: [Integer] -> Set Integer
toSet = foldl (\x acc -> insert acc x) empty

-- generates a^b for all combinations of a and b where 2 <= a,b <= 100
exponents :: [Integer]
exponents = [a^b | a <- [2..100], b <- [2..100]]

-- counts the number of such unique combinations
-- 9183
solution :: Int
solution = size $ toSet exponents

main = print solution