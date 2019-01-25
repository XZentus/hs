import System.Random

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

targetFun :: Double -> Double
targetFun x = cos(2.16327*x) + x*0.3423 - 3.0;

simplify :: Expr -> Expr
simplify e = let e' = smpl e
             in if e == e' then e else simplify e'
  where
    smpl (U UMin intern)    = let intern' = simplify intern
                              in case intern' of
                                  (N n) -> N $ -n
                                  _     -> U UMin intern'
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

minValue             = -20.0
maxValue             =  20.0
argProbability       =   0.7
functionProbability  =   0.5
mutateArg           =    0.1;
mutateNum           =    0.6;
mutateFun           =    0.2;
mutateMin           =   -2.0;
mutateMax           =    2.0;

initValueRange = (minValue, maxValue)
mutValueRange  = (mutateMin, mutateMax)

genExpr :: Int -> IO Expr
genExpr depth | depth < 1 = do
    rand <- randomIO :: IO Double
    if rand < argProbability
    then return Arg
    else randomRIO initValueRange >>= return . N

              | otherwise = do
    rand <- randomIO :: IO Double
    if rand < functionProbability
    then do
        l <- genExpr (depth - 1)
        funId <- randomRIO (1, 8) :: IO Int
        if funId <= 4
        then return $ case funId of
            1 -> U Sin l
            2 -> U Cos l
            3 -> U Tan l
            4 -> U UMin l
        else do
            r <- genExpr (depth - 1)
            return $ case funId of
                5 -> B Add l r
                6 -> B Sub l r
                7 -> B Mul l r
                8 -> B Div l r
    else if rand < argProbability
        then return Arg
        else randomRIO initValueRange >>= return . N

mutate :: Int -> Expr -> IO (Bool, Expr)
mutate depth expr = do
    rand <- randomIO :: IO Double
    case expr of
        Arg -> if rand < mutateArg
               then do
                   e' <- genExpr depth
                   return (e' /= expr, e')
               else return (False, expr)
               
        N n -> if rand < mutateNum
               then randomRIO mutValueRange >>= return . (,) True . N . (n +)
               else return (False, expr)
               
        U op l -> do
                    (mutated, l') <- mutate (depth - 1) l
                    if mutated
                    then return $ (True, U op l')
                    else if rand < mutateFun
                         then do
                             e' <- genExpr depth
                             return (e' /= expr, e')
                         else return (False, expr)
                    
        B op l r -> do
                    (mutatedl, l') <- mutate (depth - 1) l
                    (mutatedr, r') <- mutate (depth - 1) r
                    if mutatedl || mutatedr
                    then return $ (True, B op l' r')
                    else if rand < mutateFun
                         then do
                             e' <- genExpr depth
                             return (e' /= expr, e')
                         else return (False, expr)



-- test :: Int -> 
test depth populationSize = do
    e <- sequence . replicate populationSize $ (genExpr depth)
    print e
    

main :: IO ()
main = do
    return ()
