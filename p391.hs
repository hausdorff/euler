import Numeric
import Char

toBinaryStr n = showIntAtBase 2 intToDigit n ""
onesInStr s = length $ filter (\x -> x == '1') s
sk n = sum $ map (onesInStr . toBinaryStr) [0..n]
s = map sk [0..]

-- union of two SORTED lists
union [] _ = []
union _ [] = []
union (x:xs) (y:ys) = case (x `compare` y) of
  LT -> union xs (y:ys)
  GT -> union (x:xs) ys
  EQ -> x : (union xs ys)

moveList c n = map (\x -> c + x) [1..n]
possibleMoves c n = union (moveList c n) s
move0 n = map (\x -> possibleMoves x n) $ possibleMoves 0 n
notLoss n [] = False
notLoss n (x:xs) = case ((>0) $ length $ possibleMoves x n) of
  False -> notLoss n xs
  True -> True

result n = takeWhile (\x -> notLoss n x) $ move0 n