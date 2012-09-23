main = do
    let ssq = sum [x*x | x <- [1..100]]
        sqs = (sum [1..100]) ^2
    print $ sqs - ssq

