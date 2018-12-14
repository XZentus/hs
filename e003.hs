primeFactors1, primeFactors2 :: Integer -> [Integer]
primeFactors1 n = pf 3 n $ if n2 /= n then [2] else []
  where
    n2 = df 2 n
    pf d n ds | n < d          = ds
              | n == d         = n : ds
              | n `rem` d == 0 = pf (d + 2) (df d n) $ d : ds
              | otherwise      = pf (d + 2) n ds
    df d n | n `rem` d == 0 = n `div` d
           | otherwise      = n
           
           
primeFactors2 x = if leastFactor x == 1 then [x] else fdivs $ pf x []
  where
    pf n ds | n == 1 = ds
            | otherwise = let lf = leastFactor n
                          in if lf == 1 then n : ds else pf (n `div` lf) $ lf : ds
    fdivs (x:y:ss) | x == y    = fdivs $ y:ss
                   | otherwise = x : (fdivs $ y:ss)
    fdivs x                    = x

           
leastFactor :: Integer -> Integer
leastFactor x | x `rem`  2 == 0 =  2
              | x `rem`  3 == 0 =  3
              | x `rem`  5 == 0 =  5
              | x `rem`  7 == 0 =  7
              | x `rem` 11 == 0 = 11
              | x `rem` 13 == 0 = 13
              | x `rem` 17 == 0 = 17
              | x `rem` 19 == 0 = 19
              | x `rem` 23 == 0 = 23
              | x `rem` 29 == 0 = 29
              | x `rem` 31 == 0 = 31
              | otherwise   = let m = ceiling $ sqrt $ fromIntegral x + 0.5;
                                  flr = floor $ (fromIntegral m / 30) :: Integer
                              in lf1 (7 + 30 * flr - 100) m
  where
    lf1, lf2 :: Integer -> Integer -> Integer
    lf1 i m | i > m             = lf2 37 m
            | x `rem` i        == 0 = i
            | x `rem` (i +  4) == 0 = i +  4
            | x `rem` (i +  6) == 0 = i +  6
            | x `rem` (i + 10) == 0 = i + 10
            | x `rem` (i + 12) == 0 = i + 12
            | x `rem` (i + 16) == 0 = i + 16
            | x `rem` (i + 22) == 0 = i + 22
            | x `rem` (i + 24) == 0 = i + 24
            | otherwise             = lf1 (i + 30) m

    lf2 i m | i > m             = x
            | x `rem` i        == 0 = i
            | x `rem` (i +  4) == 0 = i +  4
            | x `rem` (i +  6) == 0 = i +  6
            | x `rem` (i + 10) == 0 = i + 10
            | x `rem` (i + 12) == 0 = i + 12
            | x `rem` (i + 16) == 0 = i + 16
            | x `rem` (i + 22) == 0 = i + 22
            | x `rem` (i + 24) == 0 = i + 24
            | otherwise             = lf2 (i + 30) m
            
            
            
main :: IO ()
main = do
    input <- getLine
    let arg = read input
    print $ primeFactors2 arg