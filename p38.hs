import Euler

euler38 start = test' 1 []
    where test' n acc
            | length acc >= 9   = acc
            | otherwise         = test' (n+1) (acc ++ (show $ start * n))

main = print . maximum $ filter isPandigitalString [euler38 x | x <- [1..999999999]]
