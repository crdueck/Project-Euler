import Euler

main = print $ sum $ filter (not . isSum) [1..28123]

abundants = filter isAbundant [12..]
isSum x = any isAbundant [x - a | a <- (takeWhile (<x) abundants)]
