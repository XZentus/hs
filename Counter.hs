module Counter where

data Counter a = Counter Int a

instance Show a => Show (Counter a) where
    show (Counter n a) = "Counter: " ++ show n ++ " | " ++ show a

instance Functor Counter where
    fmap f (Counter n a) = Counter (n + 1) $ f a

sumCnt :: Int -> Int -> Int
sumCnt x y = (x `max` y) + 1

instance Applicative Counter where
    pure a = Counter 0 a
    
    (Counter n1 f) <*> (Counter n2 x) = Counter (sumCnt n1 n2) $ f x

instance Monad Counter where
    (Counter n1 a) >>= f = let (Counter n2 b) = f a
                           in Counter (sumCnt n1 n2) b


--test :: Counter String
test = do
    let f1 = Counter 100500
    f1 ""
    f1 ""
    f1 ""
    