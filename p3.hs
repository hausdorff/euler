module P3 where
import Sieve

-- our target value to find largest prime factor of
target = 600851475143
--target = 13195

-- square root of integral type
sqrtInteger = floor . sqrt . (fromIntegral :: Integer -> Double)

p3 = do
     last [x | x <- primesLtN (sqrtInteger target), target `mod` x == 0];