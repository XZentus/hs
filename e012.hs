import Data.List

numberDivs :: Int -> Int
numberDivs x = 2 + sum [ 2 | d <- [(2 :: Int) .. floor $ sqrt $ fromIntegral x], x `rem` d == 0 ]

solveFunctional :: Int -> Maybe Int
solveFunctional limit = find (\x -> numberDivs x > limit) $ scanl (+) 3 [3..]

solveImperative :: Int -> Int
solveImperative limit = si 10
  where
    si n | numberDivs n > limit = n
         | otherwise            = si $ n + 1

smallPrimes = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127]

number2Factors :: Int -> [(Int, Int)]
number2Factors = group [] . fn smallPrimes
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

numberDivsFast :: Int -> Int
numberDivsFast = product . map ((1 +) . fst) . number2Factors

solveFast limit = find (\x -> numberDivsFast x > limit) $ scanl (+) 3 [3..]

main :: IO ()
main = print $ solveFast 500
