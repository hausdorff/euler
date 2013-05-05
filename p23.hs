import Data.List
import Data.Numbers.Primes
import Data.Set (Set, empty, member, insert, difference, elems)
import Array

primePowerFactors = map (\xs -> (head xs, length xs)) . group . primeFactors

sumProperDivisors n
  = product [((p^(a+1)) - 1) `div` (p - 1) | (p, a) <- primePowerFactors n] - n

toSet :: [Int] -> Set Int
toSet = foldr (\x st -> Data.Set.insert x st) empty

-- the set of abundant numbers from 1..28123
abundantNumsSet = toSet $ filter (\x -> sumProperDivisors x > x) [1..upperbound]

-- determines whether number can be created by adding two abundant numbers
abundantPair' n x
  | (member x abundantNumsSet) && (member (n-x) abundantNumsSet) = True
  | x >= n `div` 2 = False
  | otherwise = abundantPair' n (x+1)

abundantPair n = abundantPair' n 1

upperbound = 28123

solution = sum $ filter (\x -> not $ abundantPair x) [1..upperbound]
