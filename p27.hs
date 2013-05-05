import Data.Numbers.Primes

-- the polynomial x = n^2 - an - b
poly :: Int -> Int -> [Int]
poly a b = [x^2 + a * x + b | x <- [0..]]

-- evals `poly` until it produces a nonprime number
consecPrimesPoly :: Int -> Int -> [Int]
consecPrimesPoly a b = [x | x <- takeWhile isPrime $ poly a b]

-- primes less than n
primesLt :: Int -> [Int]
primesLt n = takeWhile (<n) primes

-- (total consecutive primes generated starting at n = 0, coeff a, coeff b)
--
consecPrimesAllPolys :: [(Int, Int, Int)]
consecPrimesAllPolys = [(length $ consecPrimesPoly a p, a, p) |
                        p <- primesLt 1000, a <- [(-p)..999]]

-- solution: -59231
solution :: Int
solution = a*b
  where (v, a, b) = maximum consecPrimesAllPolys

main :: IO ()
main = print $ solution