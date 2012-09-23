import Euler

comb n r = fact n / (fact r * fact (n - 4))
    where fact 1 = 1
          fact n = n * fact (n - 1)

main = print $ length $ filter (>1e6 . comb)
