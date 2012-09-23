-- Slow as hell
import Euler
import Primes
import Data.List (foldl')
import qualified Data.MemoCombinators as Memo

chainLength = Memo.arrayRange (1, 10000000) chain
    where chain 1 = 1
          chain n = 1 + (chainLength $ phi n)

primesTo = takeWhile (<40000000) primes

combine x = foldl' select (0,0) x
    where select (25,b) (_,y) = (25,b+y)
          select _ (_,y)      = (25,y)

main = print $ combine . zip (map chainLength $ primesTo) $ primesTo
