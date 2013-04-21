
-- For sanity, let's just add up digits 1-9
-- one two three four five six seven eight nine
-- 3   3   5     4    4    3   5     5     4
-- TOTAL: 36
oneToNine = 36

-- Now, 10-19
-- ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen
-- 3   6      6      8        8        7       7       9         8        8
-- TOTAL: 70
tenToNineteen = 70

-- Now, pwers of ten
-- twenty thirty forty fifty sixty seventy eighty ninety
-- 6      6      5     5     5     7       6      6
-- TOTAL: 46
pwrsOfTen = 46

-- Now, numbers 1-99:
-- 1-9          = oneToNine
-- 10-19        = tenToNineteen
-- 20,30,...,90 = pwrsOfTen * 10 (each is repeated 10 times, eg, 20,21,...,29)
-- x1,x2,...,x9 = oneToNine * 8 (digits 1-9 used for every 10 eg 20,21,...,29)
oneToNinetyNine = oneToNine + tenToNineteen + oneToNine * 8 + pwrsOfTen * 10

-- Finally, 1-1000
-- base     = 100 * oneToNine (one in one-hundred, each used 100 times)
-- hundred  = 7 * 100 * 9 ("hundred" repeated 100 times for each successive 100)
-- ands     = 99 * 9 * 3
-- one thousand = 11
-- 10 * oneToNinetyNine (one for each successive 100)
base = 100 * oneToNine
hundreds = 7 * 100 * 9
ands = 99 * 9 * 3
oneThousand = 11

total = base + hundreds + ands + oneThousand + 10 * oneToNinetyNine


main = print total