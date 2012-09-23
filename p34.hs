import Data.Char
main = print $ sum ([x | x <- [3..100000], x == sum (map (\n -> product [1..n]) (map digitToInt $ show x))])
