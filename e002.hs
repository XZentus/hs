import Control.Monad

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

sumFibs1, sumFibs2 :: Integer -> Integer
sumFibs1 limit = sum $ filter even $ takeWhile (< limit) fibs

sumFibs2 limit = sf 1 1 0
  where
    sf f1 f2 s | f1 >= limit = s
               | otherwise   = let adder = if even f1 then f1 else 0
                               in sf f2 (f1+f2) $ s + adder

main :: IO ()
main = do
    let limit = 4000000
    forM_ [sumFibs1, sumFibs2] $ \f -> print $ f limit