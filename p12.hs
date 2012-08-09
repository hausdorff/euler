module P12 where

import List
import Data.Numbers.Primes (primes)

-- NOT unique prime factors.
primeFactorTree 1 = []
primeFactorTree n = factor : primeFactorTree (n `div` factor)
             where
               factor = head $ filter xDividesN primes
               xDividesN x = (n `mod` x) == 0

-- counts number of ways prime factors can be combined to generate composites
numCompositeFactors = product . map ((1+) . length) . group . primeFactorTree

-- a triangle number T(n) = sum [1..n]. Since T(n) = T(n - 1) + n, we can
-- implement this function using dynamic programming:
triangleNumList = triangleNumList' 1 2
               where triangleNumList' acc n =
                       acc : (triangleNumList' (acc + n) (n + 1))

triangleNumsAndFactors = [(tri, numCompositeFactors tri) | tri <- triangleNumList]

main =
  print $ fst $ head $ filter ((>500) . snd) triangleNumsAndFactors