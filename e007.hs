import Data.Numbers.Primes

nthPrime :: Int -> Int
nthPrime = (primes !!) . (flip (-) 1)

main :: IO ()
main = do
    let target = 10001
    print $ nthPrime target