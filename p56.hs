import Euler
main = print . maximum $ map (sum . numToList) [a^b | a <- [1..100], b <- [1..100]]
