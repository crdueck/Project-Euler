import Euler
import System.Environment
import Data.Ratio
import Data.List (foldl1')

euler70 n = filter (\(a,b) -> isPermutation a b) $ zip [1..n] (map phi [1..n])

ratioMin :: (Integral a, Num a, Ord a) => (a,a) -> (a,a) -> (a,a)
ratioMin (a,b) (x,y)
    | a % b < x % y     = (a,b)
    | otherwise         = (x,y)

main = do
    [d] <- map read `fmap` getArgs
    print $ foldl1' ratioMin $ euler70 d
