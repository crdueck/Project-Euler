import Euler

euler179 k = euler' 0 1 0
    where euler' s n old
            | k <= n        = s
            | old == new    = euler' (s + 1) (n + 1) new
            | otherwise     = euler' s (n + 1) new
            where new = length $ divisors (n + 1)

main = print $ euler179 10000000
