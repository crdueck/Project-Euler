import Euler
import Data.Char (intToDigit)
import Data.List (sort, foldl')
import Data.Bits

fib :: Int -> Integer
fib n = snd . foldl' fib' (1, 0) . dropWhile not $ [testBit n k | k <- let s = bitSize n in [s-1,s-2..0]]
    where
        fib' (f, g) p
            | p         = (f*(f+2*g), ss)
            | otherwise = (ss, g*(2*f-g))
            where ss = f*f+g*g

p n = let xs = show n in (isPandigitalString $ take 9 xs) && (isPandigitalString . take 9 $ reverse xs)

{-main = print $ head . filter p $ map fib [2741..1000000]-}
main = print $ sum $ map fib [0..100000]
