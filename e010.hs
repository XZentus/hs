import Data.Numbers.Primes

main :: IO ()
main = do
    let limit = 2000000 :: Int
    print $ sum . takeWhile (< limit) $ primes