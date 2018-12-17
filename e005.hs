smallPrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113]

number2Factors :: Int -> [(Int, Int)]
number2Factors = reverse . group [] . fn smallPrimes
  where
    group result [] = result
    group [] (x:xs) = group [(1, x)] xs
    group res@((n, xr):rs) (x:xs) | x == xr   = group ((n+1, x) : rs)  xs
                                  | otherwise = group ((  1, x) : res) xs
    fn :: [Int] -> Int -> [Int]
    fn  _ 1 = []
    fn [] x = [x]
    fn (d:ds) x | x `rem` d == 0 = d : (fn (d:ds) $ x `div` d)
                | otherwise      =      fn    ds    x

unionFactors :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
unionFactors [] x = x
unionFactors x [] = x
unionFactors l1@(v1@(n1, f1):fs) l2@(v2@(n2, f2):ss)
    | f1 == f2  = (max n1 n2, f1) : (unionFactors fs ss)
    | f1 <  f2  = v1              : (unionFactors fs l2)
    | otherwise = v2              : (unionFactors l1 ss)

factors2Number :: [(Int, Int)] -> Integer
factors2Number = foldl (\res (n, f) -> res * (fi f) ^ (fi n) ) 1
    where fi = fromIntegral
  
  
main :: IO ()
main = do
    let limit = 20
        f = factors2Number . foldl1 unionFactors . map number2Factors
    print $ f [2 .. limit]