module Sieve where

-- basis of our sieve
basis = 2 : 3 : [5,7..]

-- set subtraction for SORTED  lists
setminus (x:xs) (y:ys) = case (compare x y) of
  LT -> x : setminus xs (y:ys)
  EQ -> setminus xs ys
  GT -> setminus (x:xs) ys
setminus xs [] = xs
setminus [] ys = ys


union (x:xs) (y:ys) = case (compare x y) of
  LT -> x : union xs (y:ys)
  EQ -> x : union xs ys
  GT -> y : union (x:xs) ys
union xs [] = xs
union [] ys = ys
joinT ((x:xs):t) = x : union xs (joinT (pairs t))
pairs ((x:xs):ys:t) = (x : union xs ys) : pairs t
gaps k s@(x:xs) | k<x = k:gaps (k+2) s
                | True = gaps (k+2) xs

primesTME = 2 : (gaps 3 $ joinT [[p*p, p*p+2*p..] | p <- primes']) where
  primes' = 3 : (gaps 5 $ joinT [[p*p, p*p+2*p..] | p <- primes'])

-- infinite series of primes
primes = sieve basis where
  sieve [] = []
  sieve (x:xs) = x : sieve (xs `setminus` [x*x, x*x+2*x..])

-- enumerated primes; since 2 is the first prime and 3 is the second, we
-- end up with a list like: [(1,2), (2,3), (3, 5), ..]
enumPrimes = [(i,p) | i <- [0,1..], p <- primesTME]

-- generates first n primes
nprimes n = [fst e | e <- enumPrimes]

-- generates all primes less than n
primesLtN n = takeWhile (<n) primesTME
