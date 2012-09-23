import Data.Char
main = print $ sum [x | x <- [2..1000000], x == (sum . map ((^5) . digitToInt) . show) x]
