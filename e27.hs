import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable

data QuadPrimesLen = QPL {a :: Int
                         ,b :: Int
                         ,n :: Int
                         }
    deriving Show

instance Eq QuadPrimesLen where
    (QPL _ _ a) == (QPL _ _ b) = a == b
    (QPL _ _ a) /= (QPL _ _ b) = a /= b

instance Ord QuadPrimesLen where
    compare (QPL _ _ a) (QPL _ _ b) = a `compare` b

getPrimeSeqLen :: Int -> Int -> State [Int] QuadPrimesLen
getPrimeSeqLen a b = do
    ps <- get
    let (newPs, result) = check ps
    put newPs
    return $ QPL a b result
  where
    check :: [Int] -> ([Int], Int)
    check = undefined

primesInit :: [Int]
primesInit = [2, 3, 5, 7, 11, 13, 17, 19, 23]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

main :: IO ()
main = do
    let range = [-999 .. 999]
        primesSeqList = flip evalState primesInit $ sequence $ zipWith getPrimeSeqLen range range
        test = map (uncurry3 QPL) [(1, 2, 3), (3, 4, 5), (10, 20, 0), (-100, 100, 100)]
    print $ foldl' max (head test) (tail test)
