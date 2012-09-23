import Data.Ratio

cFraction k
        | k > 10 = 0
        | 1/(1+1/(2*k)+1/(cFraction (k + 1)))


main = do
    print continuedFraction 1
