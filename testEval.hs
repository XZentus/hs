import Data.Time

data Expr = Arg
          | N Double
          | U UnaryOp Expr
          | B BinaryOp Expr Expr

data UnaryOp  = Sin | Cos | Tan | UMin
data BinaryOp = Add | Sub | Mul | Div

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

    eval (N n)       x = n
    
    eval (U Sin e)   x = sin $ eval e x
    eval (U Cos e)   x = cos $ eval e x
    eval (U Tan e)   x = tan $ eval e x
    eval (U UMin e)  x = - (eval e x)
    
    eval (B Add l r) x = eval l x + eval r x
    eval (B Sub l r) x = eval l x - eval r x
    eval (B Mul l r) x = eval l x * eval r x
    eval (B Div l r) x = eval l x / eval r x

testExpr = B Mul (U Sin (B Div (N 1) Arg))
                 (U Cos (B Add (N 2) (B Sub (N 3) (N 2))))

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
    time1 <- getCurrentTime
    print $ sum $ map l args -- (eval testExpr) args
    time2 <- getCurrentTime
    print $ sum $ map (eval testExpr) args -- l args
    time3 <- getCurrentTime
    putStrLn ""
    print $ diffUTCTime time2 time1
    print $ diffUTCTime time3 time2
