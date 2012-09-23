import Data.List (foldl1')
import Euler (intSqrt)

chain :: Int -> [Int]
chain n = chain' [n] $ sumDivisors n
    where chain' acc d
            | d == n       = acc
            | limit d acc  = []
            | otherwise    = chain' (d : acc) $ sumDivisors d
          sumDivisors = sum . divisors
          limit n acc = n > 1000000 || n `elem` acc

divisors :: Int -> [Int]
divisors n = divisors' [] 2
    where divisors' acc d
            | intSqrt n < d  = 1 : acc
            | n `mod` d == 0 = divisors' (d : (n `div` d) : acc) (d + 1)
            | otherwise      = divisors' acc (d + 1)

longestChain :: Int -> [Int]
longestChain n = foldl1' cmpChain (map chain [2,4..n])
    where cmpChain x y
            | length x < length y = y
            | otherwise           = x

main = print $ minimum $ longestChain 13000
