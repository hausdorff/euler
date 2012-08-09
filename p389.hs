rollingMean [] acc n = acc / n
rollingMean (x:xs) acc n = rollingMean xs (acc + x) (succ n)
mean nums = rollingMean nums 0 0

t = [1..4]
tMu = mean t

c = [1*6, 2*6..4*6]
cMu = mean c

o = [1*8, 2*8..24*8]
oMu = mean o

d = [1*12, 2*12..192*12]
dMu = mean d

i = [1*20, 2*20..2304*20]
iMu = mean i

