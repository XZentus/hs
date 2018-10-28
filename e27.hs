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
    let (newPs, result) = check ps 0
    put newPs
    return $ QPL a b result
  where
    check :: [Int] -> Int -> ([Int], Int)
    check nps n = let (nps', ip) = isPrime nps (n*n + a*n + b)
                  in if ip
                     then check nps' $ n + 1
                     else (nps', n)
    isPrime :: [Int] -> Int -> ([Int], Bool)
    isPrime p       n | n < 0      = (,) p False
    isPrime p@(f:_) n | f * f == n = (,) p False
                      | f * f >  n = let result = all (\d -> (n `rem` d) /= 0) p
                                     in (,) p result
                      | f * f <  n = let nps = getNextPrime p
                                     in isPrime nps n
    getNextPrime :: [Int] -> [Int]
    getNextPrime p@(f:_) = gnp (f + 2) p
      where
        gnp n [] = n : p
        gnp n (f:rest) | n `rem` f == 0 = gnp (n + 2) p
                       | otherwise      = gnp n rest

primesInit :: [Int]
primesInit = reverse [2, 3, 5, 7, 11, 13, 17, 19, 23]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

main :: IO ()
main = do
    let aRange = [-999 .. 999]
        bRange = [-1000 .. 1000]
        raw = flip runState primesInit $ sequence $ [getPrimeSeqLen a b | a <- aRange, b <- bRange]
        primesSeqList = fst raw
--        test = map (uncurry3 QPL) [(1, 2, 3), (3, 4, 5), (10, 20, 0), (-100, 100, 100)]
        result = foldl' max (head primesSeqList) (tail primesSeqList)
--    print $ foldl' max (head test) (tail test)
    print $ snd $ raw
    print result
    print $ a result * b result
