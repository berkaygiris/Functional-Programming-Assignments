import Prelude

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



main :: IO ()
main = print $ dayOfWeek(2018, 2, 23)
