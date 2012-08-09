module P12 where

import List

triangleNums = map (\i -> sum [1..i]) [1..]
powerset' [] = []
powerset' (x:xs) = xs : map (x:) (powerset' xs) ++ (powerset' xs)
powerset s = s : powerset' s
factors n = [x | x <- [1..(n `div` 2)], n `mod` x == 0] ++ [n]
trifactors = [factors x | x <- triangleNums]


range x y = [x..y]
baserange = [1..500]
testrange = [1..4]

prod (x:[]) = x
prod (x:xs) = x * prod xs

sumNInts n = (n * (n + 1))/2


testval n x = case n < (length $ factors x) of
  True -> Just True
  False -> Nothing

test n [] = Nothing
test n (x:xs) = case testval n x of
  Nothing -> test n xs
  Just results -> Just results
{-
test n (x:xs) = case (length $ factors x) > n of
  True -> True
  False -> test n xs
-}


{-
p12 = find (\x -> snd x >= 500) $ zip triangleNums $ map length trifactors
-}