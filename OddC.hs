data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x1) (Un x2) xs1        = Bi x1 x2 xs1
concat3OC (Bi x1 x2 xs1) xs2 xs3     = Bi x1 x2 $ concat3OC     xs1 xs2 xs3
concat3OC (Un x1) (Bi x2 x3 xs1) xs2 = Bi x1 x2 $ concat3OC (Un x3) xs1 xs2


concatOC :: OddC (OddC a) -> OddC a
concatOC (Un x) = x
concatOC (Bi xs1 xs2 rest) = concat3OC xs1 xs2 $ concatOC rest

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
    
main = test2
