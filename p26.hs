import Data.Numbers.Primes

{- Every number that does not contain prime factors 2 or 5 will be guaranteed
   to have a period. To find that, divide by increasingly large numbers
   composed of 9s, eg, 9/7, then 99/7, then 999/7, and so on. Each of these
   doesn't divide evenly, that is 9 `mod` 7 /= 0. But eventually we find that
   999999/7 does divide evenly. It gives us 142857. And that is the repeated
   part of the decimal!

   This code is designed to do find the decimal period for each number in
   2..999. We return the number with the largest period. -}

nines' n = n*9 : nines' n'
  where n' = (n*10+1)

-- generates increasingly large numbers consisting of only 9's, eg, 9, 99, 999,
-- and so on.
nines = nines' 1

-- m evenly divides n?
evenlyDivides n m = n `mod` m == 0

-- find prime factors, make sure none of them is 2 or 5.
primeFactorTest n = (==0) $ length $ filter (\x -> x == 2 || x == 5) $ primeFactors n

-- get the smallest number consisting of all 9's that evenly divides our number
period n = head $ take 1 [x `div` n | x <- nines, x `evenlyDivides` n]

solution = snd $ maximum [(period x, x) | x <- (filter primeFactorTest [2..999])]