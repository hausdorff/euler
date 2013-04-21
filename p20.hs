-- uses Haskell's nice arbitrary-sized integer multiplication to simply
-- generate 100!. We then convert that number to a list of digits and sum

import Char

factorial n = product [1..n]

-- takes integral, produces list of digits
digits 0 = []
digits n = (digits (n `div` 10)) ++ [n `mod` 10]
--[n `mod` k | k <- [10^i | i <- [1..]]]

solution = sum $ digits $ factorial 100

-- a friend's clever solution using nice libraries I didn't know about
solution' = sum [digitToInt x | x <- (show $ product [1..100])]

main = print solution