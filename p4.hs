module P4 where

palindrome n = test nstr (reverse nstr) l where
  nstr = show n
  l = (length nstr) `div` 2
  test [] _ _ = True
  test _ _ 0 = True
  test (x:xs) (y:ys) i = case (compare x y) of
    LT -> False
    GT -> False
    EQ -> test xs ys (i-1)

p4 = maximum [x*y | x <- [100..999], y <- [100..999], palindrome (x*y)]