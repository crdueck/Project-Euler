import Data.List (sort, permutations)
main = print $ (sort $ permutations "0123456789") !! 999999
