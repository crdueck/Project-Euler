module Euler where
import Primes (primes)
import Control.Monad (filterM)
import Control.Arrow ((&&&))
import Data.List (group, nub, sort)
import Data.Char (intToDigit)

fibonacci :: [Integer]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime p = p == 2 || (odd p && all (\x -> p `mod` x /= 0) [3,5..intSqrt p])

divisors :: Integer -> [Integer]
divisors = nub . map product . powerset . primeFactors

primeFactors :: Integer -> [Integer]
primeFactors n = if isPrime n then [n] else pFactor' primes
    where pFactor' (p:xs)
            | p > intSqrt n     = []
            | n `mod` p == 0    = p : (primeFactors (n `div` p))
            | otherwise         = pFactor' xs

intSqrt :: Integral a => a -> a
intSqrt = floor . sqrt . fromIntegral

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

isPermutation :: Integer -> Integer -> Bool
isPermutation n m = sort (show n) == sort (show m)

numToList :: Integer -> [Integer]
numToList n = numToList' n []
    where numToList' n acc
            | n < 10    = n : acc
            | otherwise = numToList' (n `div` 10) ((n `rem` 10) : acc)

numToListR :: Integer -> [Integer]
numToListR n = (n `rem` 10) : numToListR (n `div` 10)

isPandigital :: Integer -> Bool
isPandigital = isPandigitalString . show

isPandigitalString :: String -> Bool
isPandigitalString s = len <= 10 && sort s == xs
    where xs  = ['1'..intToDigit len]
          len = length s

powerset :: [a] -> [[a]]
powerset = filterM (\_ -> [True, False])

runLengthEncode :: Eq a => [a] -> [(Integer,a)]
runLengthEncode = map (fromIntegral . length &&& head) . group

phi :: Integer -> Integer
phi 0 = 0
phi n = product $ map phi' pFactorization
    where phi' (k, p) = p^(k - 1) * (p - 1)
          pFactorization = runLengthEncode $ primeFactors n
