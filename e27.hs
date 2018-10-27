import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable

primesInit :: [Int]
primesInit = reverse [2, 3, 5, 7, 11, 13, 17]

extendPrimes :: Int -> [Int] -> [Int]
extendPrimes n (p:ps) = undefined

getPrimeSeqLen :: undefined

maxCoeffs :: [Int] -> [Int] -> [(Int, Int, Int)]
maxCoeffs as bs = [(a, b, getPrimeSeqLen a b) | a <- as, b <- bs]

get3 :: (a, b, c) -> c
get3 (_, _, c) = c

routine :: Int -> Int -> State [Int] Int
routine a b = do

main :: IO ()
main = do
--  let range = [-999 .. 999]
--  print $ get3 $ foldl' (\f s -> if get3 f > get3 s
--                                 then f else s) $ maxCoeffs range range
