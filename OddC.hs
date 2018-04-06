data OddC a = Un a | Bi a a (OddC a) deriving (Eq,Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x1) (Un x2) xs1        = Bi x1 x2 xs1
concat3OC (Bi x1 x2 xs1) xs2 xs3     = Bi x1 x2 $ concat3OC     xs1 xs2 xs3
concat3OC (Un x1) (Bi x2 x3 xs1) xs2 = Bi x1 x2 $ concat3OC (Un x3) xs1 xs2

main = do
    let tst1 = Bi 'a' 'b' (Un 'c')
        tst2 = Bi 'd' 'e' (Bi 'f' 'g' (Un 'h'))
        tst3 = Bi 'i' 'j' (Un 'k')
        res = concat3OC tst1 tst2 tst3
    print $ res
    putStrLn $ "Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))"
    print $ res == Bi 'a' 'b' (Bi 'c' 'd' (Bi 'e' 'f' (Bi 'g' 'h' (Bi 'i' 'j' (Un 'k')))))
