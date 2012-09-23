import qualified Data.MemoCombinators as Memo
import Data.List (foldl1')

main = print $ snd . foldl1' max $ zip (map seqLength [1..999999]) [1..]

seqLength = Memo.arrayRange (1, 1000000) seqLength'
    where seqLength' 1 = 1
          seqLength' n
            | odd n     = 1 + seqLength (3*n+1)
            | otherwise = 1 + seqLength (n `div` 2)
