nextDivNum, prevDivNum :: Int -> Int -> Int
nextDivNum n m | n `rem` m == 0 = n 
               | otherwise      = n + m - n `rem` m

prevDivNum n m = n - n `rem` m

mkDivRange :: Int -> Int -> Int
           -> ( Int  -- begin
              , Int  -- end
              , Int) -- number of values
mkDivRange from to d = let f = nextDivNum from d
                           t = prevDivNum to d
                           n = 1 + (t - f) `div` d
                       in (f, t, n)

rangeSum :: Int -> (Int, Int, Int) -> Int
rangeSum d (f, t, n) | even n    = f + rangeSum d (f + d, t, n - 1)
                     | otherwise = midValue * n
  where
    midValue = (f + t) `div` 2

main :: IO ()
main = do
    let from = 1
        to = 999
        [s3, s5, s15] = [rangeSum x (mkDivRange from to x) | x <- [3, 5, 15]]
    print $ s3 + s5 - s15