import Data.List

p a b = sort (show a) == sort (show b)
main = print $ head [x | x <- [1..], all (p x) [x*n | n <- [2..6]]]
