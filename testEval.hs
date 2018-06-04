import Data.Time

data Expr = Arg
          | N Double
          | U UnaryOp Expr
          | B BinaryOp Expr Expr
    deriving Eq

data UnaryOp  = Sin | Cos | Tan | UMin
    deriving Eq
data BinaryOp = Add | Sub | Mul | Div
    deriving Eq

class EvalExpr e where
    eval :: e -> Double -> Double

instance Show UnaryOp where
    show Sin  = "sin"
    show Cos  = "cos"
    show Tan  = "tan"
    show UMin = "-"

instance Show BinaryOp where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

instance Show Expr where
    show Arg                            = "x"
    show (N n)                          = show n
    show (U UMin (N n))                 = show $ -n
    show (U op e)                       = show op ++ "(" ++ show e ++ ")"
    show (B op l@(B _ _ _) r@(B _ _ _)) = "(" ++ show l ++ ")" ++ " " ++ show op ++ " " ++ "(" ++ show r ++ ")"
    show (B op l@(B _ _ _) r)           = "(" ++ show l ++ ")" ++ " " ++ show op ++ " " ++        show r
    show (B op l           r@(B _ _ _)) =        show l ++        " " ++ show op ++ " " ++ "(" ++ show r ++ ")"
    show (B op l           r)           =        show l ++        " " ++ show op ++ " " ++        show r

instance EvalExpr Expr where
    eval Arg         x = x

    eval (N n)       _ = n
    
    eval (U Sin e)   x = sin $ eval e x
    eval (U Cos e)   x = cos $ eval e x
    eval (U Tan e)   x = tan $ eval e x
    eval (U UMin e)  x = - (eval e x)
    
    eval (B Add l r) x = eval l x + eval r x
    eval (B Sub l r) x = eval l x - eval r x
    eval (B Mul l r) x = eval l x * eval r x
    eval (B Div l r) x = eval l x / eval r x

testExpr :: Expr
testExpr = B Mul (U Sin (B Div (N 1) (B Add Arg (N 4.5))))
                 (U Cos (B Add (N 2) (B Sub (B Mul Arg (N 3)) (U Cos (N 2)))))

simplify :: Expr -> Expr
simplify e = let e' = smpl e
             in if e == e' then e else simplify e'
  where
    smpl (U op intern)      = let intern' = simplify intern
                              in case intern' of
                                  (N n) -> N $ eval (U op (N n)) undefined
                                  _     -> U op intern'
    smpl expr@(B op l r) = let l' = simplify l
                               r' = simplify r
                           in opt (B op l' r')
      where
        opt expr@(B _ (N _) (N _)) = N $ eval expr undefined
        opt e                      = e

    smpl expr = expr


showExprResult :: Expr -> Double -> String
showExprResult e x = "x = " ++ show x ++ "\n" ++ show e ++ " = " ++ show (eval e x)

makeLambda :: Expr -> Double -> Double
makeLambda Arg = id
makeLambda (N n)       = const n
makeLambda (U Sin e)   = \x -> sin $ makeLambda e x
makeLambda (U Cos e)   = \x -> cos $ makeLambda e x
makeLambda (U Tan e)   = \x -> tan $ makeLambda e x
makeLambda (U UMin e)  = \x -> - (makeLambda e x)
makeLambda (B Add l r) = \x -> makeLambda l x + makeLambda r x
makeLambda (B Sub l r) = \x -> makeLambda l x - makeLambda r x
makeLambda (B Mul l r) = \x -> makeLambda l x * makeLambda r x
makeLambda (B Div l r) = \x -> makeLambda l x / makeLambda r x

main :: IO ()
main = do
    print testExpr
    let l = makeLambda testExpr
        args = [1 .. 1000000]
        s = simplify testExpr
        s' = makeLambda s
    print s
    time1 <- getCurrentTime
    print $ sum $ map (eval s) args
    time2 <- getCurrentTime
    print $ sum $ map s' args
    time3 <- getCurrentTime
    putStrLn ""
    print $ diffUTCTime time2 time1
    print $ diffUTCTime time3 time2
