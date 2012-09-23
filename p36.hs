import Data.Digits (digits, unDigits)
import Data.List
import Euler (isPalindrome)
import Control.Arrow ((&&&))

convertBase :: Integral a => a -> a -> [a] -> [a]
convertBase from to = digits to . unDigits from

ten2two :: Integer -> String
ten2two x = foldl1' (++) . map show $ convertBase 10 2 [x]

main = print $ foldl1' (+) $ filter p [1,3..1000000]
    where p = (\(a,b) -> a && b) . (isPalindrome . show &&& isPalindrome . ten2two)
