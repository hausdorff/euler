x = sqrt 5
y = sqrt 65
z = sqrt 68

semiperim s1 s2 s3 = (s1 + s2 + s3) / 2

-- traditional area of triangle based on length of all three sides
areaTriangle s1 s2 s3 = sqrt( semip * (semip - s1) * (semip - s2) * (semip - s3) )
  where semip = semiperim s1 s2 s3

-- special triangle for the problem
striangle b c = areaTriangle (sqrt (1+b**2)) (sqrt (1+c**2)) (sqrt (b**2+c**2))

-- all combinations in the set of {1, ..., n}**2
combinations n = [(i, j) | i <- [1..n], j <- [i..n]]

s n = sum $ filter (<n) $ map (\(x,y) -> striangle x y) $ combinations n

{-
sseq x = [(x,j) | j <- [1..]]
s1d n i = takeWhile (\(x,y) -> (striangle x y) < n) $ sseq i
s n = sum $ map (\x -> length x) $ takeWhile (\x -> (length x) > 0) $ map (\x -> s1d n x) [1..]
-}