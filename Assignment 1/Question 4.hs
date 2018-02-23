leap :: Integer -> Bool
leap y = if mod y 100 == 0 then mod y 400 == 0 else mod y 4 == 0

daysInMonth :: Integer -> Integer -> Integer
daysInMonth m y
    | m == 4 || m == 6 || m == 9 || m == 11 = 30
    | m == 2 = if leap y then 29 else 28
    | otherwise = 31

sundays2 :: Integer -> Integer -> Integer
sundays2 start end = sundays' start 1 0 2
    where
        sundays' :: Integer -> Integer -> Integer -> Integer -> Integer
        sundays' y m acc weekday
            | y > end = acc
            | otherwise = if nextWeekday `mod` 7 == 0 then sundays' nextY nextM (acc + 1) nextWeekday else sundays' nextY nextM acc nextWeekday
            where
                nextY = if m == 12 then y + 1 else y
                nextM = if m < 12 then m + 1 else 1
                nextWeekday = ((daysInMonth m y) `mod` 7) + weekday

main :: IO ()
main = print $ sundays2 1901 2000
