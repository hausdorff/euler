-- uses Haskell's nice arbitrary-sized integer multiplication to simply
-- generate 100!. We then convert that number to a list of digits and sum

factorial n = product [1..n]

-- takes integral, produces list of digits
digits 0 = []
digits n = (digits (n `div` 10)) ++ [n `mod` 10]
--[n `mod` k | k <- [10^i | i <- [1..]]]

solution = sum $ digits $ factorial 100

main = print solution