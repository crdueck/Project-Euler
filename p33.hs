import Data.Ratio
import Data.List
import Data.Char

numStringToNum ys = go $ reverse ys
    where go [] = 0
          go (x:xs) = digitToInt x + 10 * go xs

set n = map digitToInt $ show n

{-main = print $ filter isCurious [(n,d) | d <- [10..99], n <- [10..d], d /= n]-}
