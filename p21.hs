import Data.List
import Data.Numbers.Primes

-- helper for method d
d' n 0 = d' n 1
d' n acc
    | acc > (n `div` 2) = []  -- boundary condition
    | n `mod` acc == 0  = acc : (d' n (acc+1))  -- is evenly divisible
    | otherwise         = d' n (acc+1)

-- sum of numbers [1..n/2] that evenly divide into n
d n = sum $ d' n 0

isAmicableNum n
    | db == n && da /= n = True
    | otherwise          = False
    where da = d n
          db = d da

-- initial slow solution
solution = filter isAmicableNum [1..10000]


pairs n = [(x,y) | x <- [1..n], y <- [1..x]]

main = print $ sum solution



{- Much faster solution using number theory I don't quite understand! Someone
   named `ijontichy` wrote this one. -}
primePowerFactors = map (\xs -> (head xs, length xs)) . group . primeFactors

sumProperDivisors n
  = product [((p^(a+1)) - 1) `div` (p - 1) | (p, a) <- primePowerFactors n] - n

amicablePairs n = cullPairs $ map generatePairs [2..n]
  where generatePairs m   = let s = sumProperDivisors m
                            in if m < s then (m, s) else (s, m)
        cullPairs []      = []
        cullPairs (x:xs)  = if x `elem` xs then x : cullPairs xs
                            else cullPairs xs

prob21 = sum $ map (\(x,y) -> x + y) $ amicablePairs 9999