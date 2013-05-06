pence :: [Int]
pence = [1, 2, 5, 10, 20, 50, 100, 200]

-- if == target, +1
-- if < target, add a coin
-- if > target, return
-- we could start at 0 and add to 200, or we could start at 200 and subtract to
-- 0. They are equivalent.
mkChng (x:xs) 0 = 1
mkChng [] _     = 0
mkChng (x:xs) total | total > 0 = (mkChng (x:xs) (total-x)) + (mkChng xs total)
                    | total < 0 = 0

solution = mkChng pence 200

main = print solution