fb n1 n2 term = (n1,term) : fb (n1+n2) n1 (term+1)
fibo = (1,1) : fb 1 1 2

solution = snd $ head $ take 1 [(x,y) | (x,y) <- fibo, length (show x) == 1000]