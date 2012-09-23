module Primes where

primes :: [Integer]
primes = 2 : ([3,5..] `minus` joinL [[p*p, p*p+2*p..] | p <- primes'])
  where
    primes' = 3 : ([5,7..] `minus` joinL [[p*p, p*p+2*p..] | p <- primes'])

joinL ((x:xs):t) = x : union xs (joinL t)

union :: Ord a => [a] -> [a] -> [a]
union  xs     []    = xs
union  []     ys    = ys
union (x:xs) (y:ys) = case (compare x y) of
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys
           GT -> y : union (x:xs)  ys

minus :: Ord a => [a] -> [a] -> [a]
minus (x:xs) (y:ys) = case (compare x y) of
           LT -> x : minus  xs  (y:ys)
           EQ ->     minus  xs     ys
           GT ->     minus (x:xs)  ys
minus  xs     _     = xs
