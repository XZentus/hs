import Control.Applicative

data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x1) (Un x2) xs1        = Bi x1 x2 xs1
concat3OC (Bi x1 x2 xs1) xs2 xs3     = Bi x1 x2 $ concat3OC     xs1 xs2 xs3
concat3OC (Un x1) (Bi x2 x3 xs1) xs2 = Bi x1 x2 $ concat3OC (Un x3) xs1 xs2

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi xs1 xs2 rest) = concat3OC xs1 xs2 $ concatOC rest

instance Functor OddC where
    fmap f (Un x1) = Un $ f x1
    fmap f (Bi x1 x2 xs1) = Bi (f x1) (f x2) $ fmap f xs1

instance Applicative OddC where
    pure = Un
--  (<*>) :: f (a -> b) -> f a -> f b
    (Un f) <*> (Un x1) = Un $ f x1
    f'@(Un f) <*> (Bi x1 x2 rest) = Bi (f x1) (f x2) $ f' <*> rest
    --(Bi f1 f2 rest) <*> x'@(Un x1) = Bi (f1 x1) (f2 x1) $ rest <*> x'
    (Bi f1 f2 fs1) <*> xs = let t1 = f1 <$> xs
                                t2 = f2 <$> xs
                                rest  = fs1 <*> xs
                            in concat3OC t1 t2 rest
instance Monad OddC where
    return = pure
    (Un x1)         >>= f = f x1
    (Bi x1 x2 fs1) >>= f = let t1 = f x1
                               t2 = f x2
                               rest = fs1 >>= f
                           in concat3OC t1 t2 rest

test1 = do
    let tst1 = Bi 'a' 'b' (Un 'c')
        tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
        tst3 = Bi 'i' 'j' (Un 'k')
        res = concat3OC tst1 tst2 tst3
    print $ res
    putStrLn $ "Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))"
    print $ res == Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
    
test2 = do
    print $ concatOC $ Un (Un 42)
    putStrLn $ "Un 42"
    print $ (concatOC $ Un (Un 42)) == (Un 42)
    let tst1 = Bi 'a' 'b' (Un 'c')
        tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
        tst3 = Bi 'i' 'j' (Un 'k')
        res = Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
    print $ res
    putStrLn $ "Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))"
    print $ res == Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))

test3 = do
    let tst1 = Bi 10 20 (Un 30)
        tst2 = Bi 1 2 (Bi 3 4 (Un 5))
        res1 = do {x <- tst1; y <- tst2; return (x + y)}
        pat1 = Bi 11 12 (Bi 13 14 (Bi 15 21 (Bi 22 23 (Bi 24 25 (Bi 31 32 (Bi 33 34 (Un 35)))))))
        res2 = do {x <- tst2; y <- tst1; return (x + y)}
        pat2 = Bi 11 21 (Bi 31 12 (Bi 22 32 (Bi 13 23 (Bi 33 14 (Bi 24 34 (Bi 15 25 (Un 35)))))))
        printCase r p = print res1 >>= \_ -> print pat1 >>= \_ -> print $ res1 == pat1
    printCase res1 pat1
    printCase res2 pat2

main = test3
