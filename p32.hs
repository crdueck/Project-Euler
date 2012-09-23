import Euler
import Data.Char
import Data.List

p :: Int -> Bool
p n = n == (sum . map (fact . digitToInt) $ show n)
    where fact 0 = 1
          fact n = n * fact (n - 1)

main = print $ sum $ filter p [1..1000000]
