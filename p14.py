from collections import defaultdict


collatz_nums = defaultdict(lambda:-1)

def memoized_collatz_cardinality(n):
    count = 0
    curr_n = n
    while True:
        count += 1
        if curr_n == 1:
            collatz_nums[n] = count - 1
            return count
        
        c = collatz_nums[curr_n]
        if c != -1:
            return count + c
        
        if curr_n % 2 == 0:
            curr_n = curr_n / 2
        else:
            curr_n = curr_n * 3 + 1
        


if __name__ == '__main__':
    max_i = (0,0)
    for i in range(1,1000000):
        if i % 10000 == 0:
            print i
        result = memoized_collatz_cardinality(i)
        if result > max_i[1]:
            max_i = (i, result)

    print max_i
