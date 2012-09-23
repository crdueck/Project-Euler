euler8 x = max (map (*) (take 5 x)) (euler8 (tail x))

main = do
    file <- readFile "/tmp/num.txt"
    print $ euler8 file
