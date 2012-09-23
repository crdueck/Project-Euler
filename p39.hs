type Triple = (Int, Int, Int)

genTriangle :: (Int, Int) -> Triple
genTriangle (m, n) =
    (m^2 - n^2, 2*m*n, m^2 + n^2)

findTriples :: Int -> [Triple]
findTriples p =


main = do
    print "hello"

