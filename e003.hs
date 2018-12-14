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
    pf n ds | n <= 1 = ds
            | otherwise = let lf = leastFactor n
                          in if lf == 1 then n : ds else pf (n `div` lf) $ lf : ds
    fdivs (x:y:ss) | x == y    = fdivs $ y:ss
                   | otherwise = x : (fdivs $ y:ss)
    fdivs x                    = x

smallPrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113]

leastFactor :: Integer -> Integer
leastFactor x = case sdv smallPrimes of
                    Just x  -> x
                    Nothing -> let m = ceiling $ sqrt $ fromIntegral x + 0.5;
                               in lf1 7 m
  where
    sdv [] = Nothing
    sdv (d:ds) = if x `rem` d == 0 then Just d else sdv ds
    lf1 :: Integer -> Integer -> Integer
    lf1 i m | i >= m             = x
            | otherwise = case sdv (map (+i) [0,4,6,10,12,16,22,24]) of
                             Just x  -> x
                             Nothing -> lf1 (i + 30) m 

main :: IO ()
main = do
    input <- getLine
    let arg = read input
    print $ primeFactors2 arg
