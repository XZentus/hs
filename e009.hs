import Data.List
import Control.Monad

solve :: Int -> [(Int, Int, Int)]
solve s = do
    a <- [1 .. s]
    b <- [a .. s - a]
    let c = s - a - b
    guard $ c >= b && a*a + b*b == c*c
    return (a, b, c)

main :: IO ()
main = do
    print $ solve 1000