main = print $ product $ map d (1:[10^x | x <- [1..6]])
    where d   = (set !!) . (\x -> x - 1)
          set = ['0'..'9'] ++ (join ['1'..'9'])
          join xs = [x:y | x <- xs, y <- xs]


