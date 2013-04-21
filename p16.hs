-- integral to list of digits
digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- sums all digits in an integral
sumdigits n = sum $ digits n

main = do
  print $ sumdigits (2^15)
  print $ sumdigits (2^1000)
