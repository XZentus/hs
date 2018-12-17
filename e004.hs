import Control.Monad

isPal6Digit :: Int -> Bool
isPal6Digit x | x < 100000 || x > 999999 = False -- error "6-digit numbers only"
              | x `rem` 10 == 0          = False
              | otherwise                = let [d1, d2, d3, d4, d5, d6] = digits [] x
                                           in d1 == d6 && d2 == d5 && d3 == d4
  where
    digits ds n | n <= 0    = ds
                | otherwise = digits (n `rem` 10 : ds) $ n `div` 10

solveFunctional :: (Int, Int, Int)
solveFunctional = foldl1 foldFun palList
  where
    foldFun a@(_,_,pa) b@(_,_,pb) | pa > pb   = a
                                  | otherwise = b
    palList = do
        n1 <- [999, 998 .. 100]
        n2 <- [n1, n1-1 .. 100]
        let prod = n1 * n2
        guard $ isPal6Digit prod
        return $ (,,) n1 n2 prod

solveImperative :: (Int, Int, Int)
solveImperative = loopOut (0, 0, 100000) 999
  where 
    less (_,_,pa) (_,_,pb) = pa < pb
    loopOut m@(_,_,pm) n1 | prod > pm = let r1 = loopIn  m  n1 n1
                                            r2 = loopOut r1 $ n1 - 1
                                        in if less r1 r2 then r2 else r1
                          | prod < pm = m
                          | otherwise = loopOut m $ n1 - 1
      where
        prod = n1 * n1

    loopIn m@(_,_,pm) n1 n2 | prod > pm = if isPal6Digit prod then (n1, n2, prod) else loopIn m n1 $ n2 - 1
                            | otherwise = m
      where
        prod = n1 * n2

main :: IO ()
main = do
    print solveFunctional
    print solveImperative