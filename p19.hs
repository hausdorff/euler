import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

totalDays = diffDays (fromGregorian 2000 12 31) (fromGregorian 1900 0 1)

jan = [1..31]
feb = [1..28]
febly = [1..29]
mar = [1..31]
apr = [1..30]
may = [1..31]
jun = [1..30]
jul = [1..31]
aug = [1..31]
sept = [1..30]
oct = [1..31]
nov = [1..30]
dec = [1..31]
leapYear = jan ++ febly ++ mar ++ apr ++ may ++ jun ++ jul ++ aug ++ sept ++
           oct ++ nov ++ dec
normalYear = jan ++ feb ++ mar ++ apr ++ may ++ jun ++ jul ++ aug ++ sept ++
             oct ++ nov ++ dec

-- generates [1,2,3,4,5,6,7,1,2,3,4,5,6,7..]
infWeeks = [ i `mod` 8 | i <- [1..], i `mod` 8 /= 0]

infYears :: Int -> [[Int]]
infYears year =
  case (year `mod` 4 == 0, year `mod` 100 == 0, year `mod` 400 == 0) of
    -- not a leap year, not a century year
    (False, False, _) -> normalYear : (infYears (year + 1))
    -- leap year, not a century year
    (True, False, _) -> leapYear : (infYears (year + 1))
    -- leap year, but not a leap century
    (_, True, False) -> normalYear : (infYears (year + 1))
    -- leap year, and leap century
    (_, True, True) -> leapYear : (infYears (year + 1))

-- don't know when jan 1 1901 is; therefore, we drop the number of days equal
-- to the length of the year 1900
daysFirstYear = length $ concat $ take 1 $ infYears 1900

numYears = 101 -- not 99 because we're dropping the first year

-- generates tuples matching up the number of the day of the week with the
-- number of the day of the month. So (1,3) would be a monday on the third of
-- some month. Thus, when it's (7,1) it would be Sunday on the first of the
-- month
weeksYears daysToDrop nyears = drop daysToDrop $ zip infWeeks $ concat $
                    take nyears$ infYears 1900

solution = length $ filter (\(x,y) -> x == 7 && y == 1) $
           weeksYears daysFirstYear numYears

main = print $ solution


{- A better solution, using libraries that I didn't know about -}

solution' = length $ filter (\(a, b, c)->(c==7))
            [toWeekDate$fromGregorian a b 1|a<-[1901..2000], b<-[1..12]]