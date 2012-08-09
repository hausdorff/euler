from __future__ import division
import sys
from math import sqrt

# Simple factoring
def factor(n, noduplicates = True):
    intn = int(n)
    factors = {}
    lastfactor = n
    i = 0
    
    # 1 is a special case
    if n == 1:
        return {1: 1}
    
    while 1:
        i += 1
        
        # avoid duplicates like {1: 3, 3: 1}
        if noduplicates and lastfactor <= i:
            break
        
        # stop when i is bigger than n
        if i > n:
            break
        
        if n % i == 0:
            factors[i] = n / i
            lastfactor = n / i
    
    return factors

def triangle_num (n):
    return (n * (n+1)) / 2

def main ():
    i = 0
    while True:
        tr = triangle_num(i)
        f = factor(tr)
        if len(f)*2 > 500:
            print 'COW'
            print i, tr
            sys.exit()
        if i % 1000 == 0:
            print i, tr, len(f)
        i += 1
        

if __name__ == '__main__':
    main()
