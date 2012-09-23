import Euler

main = print $ head . dropWhile (\(_,x) -> x < 1000) $ zip fibonacci (map (length . show) fibonacci)
