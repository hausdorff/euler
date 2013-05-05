-- takes integral, produces list of digits
digits 0 = []
digits n = (digits (n `div` 10)) ++ [n `mod` 10]

-- sum of the digits, where each digit is raised to the fifth power
fifthPowSum :: [Integer] -> Integer
fifthPowSum = foldl (\acc x -> x^5 + acc) 0

-- all powers not including 1 whose fifth-power-sum is the number itself.
-- the magic number 500,000 is an upper bound on the largest possible number
-- for which this property can hold. (not bounding makes it take forever)
solution :: [Integer]
solution = [x | x <- [2..500000], x == (fifthPowSum $ digits x)]

main = print (sum solution, solution)