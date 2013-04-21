import math

def nCr (n,r):
    f = math.factorial
    return f(n) / f(r) / f(n-r)

# computes paths for nxn matrix
def paths (n):
    return 2*nCr((2*n)-1, n)

#137846528820
if __name__ == '__main__':
    print(paths(1))
    print(paths(2))
    print(paths(3))
    print(paths(4))
    print(paths(5))
    print(paths(6))
    print(paths(20))
