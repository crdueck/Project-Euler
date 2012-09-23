import Data.List

isPrize :: String -> Bool
isPrize xs = isPrize' xs 0 0
    where isPrize' _ 3 _ = False
          isPrize' _ _ 2 = False
          isPrize' (x:xs) absent lates
            | x == 'L'   = isPrize' xs 0 (1 + lates)
            | x == 'A'   = isPrize' xs (1 + absent) lates
            | otherwise  = isPrize' xs 0 lates
          isPrize' _ _ _ = True

main = print $ length . filter isPrize . sequence . take 15 $ cycle ["OLA"]
