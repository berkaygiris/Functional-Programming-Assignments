dayOfWeek :: (Integer, Integer, Integer) -> Integer

dayOfWeek (y, m, d) = ( d + c1 + k + k2 + j1 + j2 ) `mod` 7
    where
        (m', y') = if m < 3 then (m + 12, y - 1) else (m, y)
        j = y' `div` 100
        c1 = floor (fromIntegral (13 * (m' + 1)) / 5.0)
        k = y' `mod` 100
        k2 = floor(fromIntegral(k) / 4)
        j1 = floor(fromIntegral(j) / 4)
        j2 = 5 * j

sundays1 :: Integer -> Integer -> Integer
sundays1 start end = sundays' start 1 0
    where
        sundays' :: Integer -> Integer -> Integer -> Integer
        sundays' y m acc
            | y > end = acc
            | otherwise = if dayOfWeek(y, m, 1) == 1 then sundays' nextY nextM (acc + 1) else sundays' nextY nextM acc
            where
                nextY = if m == 12 then y + 1 else y
                nextM = if m < 12 then m + 1 else 1


main :: IO ()
main = print $ sundays1 1901 2000
