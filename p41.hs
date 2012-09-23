import Euler
import Data.List (permutations)

main = print $ maximum . filter (isPrime . read) $ permutations "1234567"
