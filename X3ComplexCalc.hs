import Data.List
import Data.Ratio
import Text.Printf

data Factory = Factory FactoryName [ProdUsing]
    deriving Show
data Ware = Ware WareName Prices
    deriving (Show, Eq)
data Prices = Prices Double Double Double   
    deriving (Show, Eq)
data ProdUsing = Prod Ware Rational 
    deriving Show

type FactoryName = String
type WareName = String
type Complex = [Factory]

complex :: Complex
complex = concatMap (uncurry replicate . (\(n, f) -> (n, findWFactory f)))
    [ (,) 2 "Пар Завод ракет \"Булава\" L"
    , (,) 1 "Пар Завод ракет \"Томагавк\" XL"
    , (,) 2 "Пар Соевая фабрика XL"
    , (,) 2 "Пар Соевая ферма XL"
    , (,) 1 "Рудная шахта XL 1-15-30"
    , (,) 1 "Рудная шахта XL 1-3-30"
    , (,) 1 "Кремниевая шахта XL 1-58-20"
    ]

complexWares = calcComplex complex
printResults = mapM_ printPU complexWares   

printPU = printProdUsing h

printBalance :: [ProdUsing] -> Integer -> IO ()
printBalance wares i = pb i 0 0 0 wares
  where pb _ smin smed smax [] = do
            putStrLn "------------------------------------------------------\n"
            printf "\t%12.2f\t%12.2f\t%12.2f\n" smin smed smax
        pb i smin smed smax ((Prod (Ware wn (Prices min med max)) r):pf) = do
            putStr $ wn ++ ":\n\t"
            let num = numerator r
                den = denominator r
                val = (fromIntegral (num * i)) / (fromIntegral den)
                vmed = val * med
                (vmin, vmax) = if val > 0 then (val * min, val * max) else (val * max, val * min)
                (nmin, nmed, nmax) = (smin + vmin, smed + vmed, smax + vmax)
            printf "\t%12.2f\n" val
            printf "\t%12.2f\t%12.2f\t%12.2f\n" vmin vmed vmax
            pb i nmin nmed nmax pf

printProdUsing :: Integer -> ProdUsing -> IO ()
printProdUsing i (Prod (Ware wn _) r) = do
    putStr $ wn ++ ":\n\t"
    let num = numerator r
        den = denominator r
    print $ (fromIntegral (num * i)) / (fromIntegral den)

calcComplex :: Complex -> [ProdUsing]
calcComplex = cc []
  where
    cc :: [ProdUsing] -> Complex -> [ProdUsing]
    cc pu [] = pu
    cc pu (f@(Factory fn wares) : fs) = cc (foldr merge pu wares) fs
    merge :: ProdUsing -> [ProdUsing] -> [ProdUsing]
    merge i            []                                = [i]
    merge i@(Prod w v) (p@(Prod w' v') : ws) | w == w'   = (Prod w $ v + v') : ws
                                             | otherwise = p : (merge i ws)
                                             

hms h m s = s + m * 60 + h * 60 * 60
h = 60 * 60
m = 60

errorWare n = Ware ("Ошибка: не найдено " ++ n) $ Prices 999999999 999999999 999999999
errorFactory n = Factory ("Ошибка: не найдено " ++ n) [ Prod (errorWare n) (-999999999 % (hms  1  0  0)) ]

waresDB :: [Ware]
waresDB = [ makeWare "Батареи"                              12    16    20
          , makeWare "Руда"                                 50   128   206
          , makeWare "Кремниевые пластины"                 232   504   776
          , makeWare "Тяжелая ракета \"Томагавк\""       20888 22460 24032
          , makeWare "Заградительная ракета \"Булава\""  26954 33692 40430
          , makeWare "Соевая мука"                         204   364   524
          , makeWare "Соевые бобы"                          14    28    42
          ]

makeWare :: WareName -> Double -> Double -> Double -> Ware
makeWare name min med max = Ware name $ Prices min med max

findWare :: WareName -> Maybe Ware
findWare n = find cond waresDB
  where cond (Ware w _) = w == n

findWErr :: WareName -> Ware
findWErr n = case (findWare n) of
                  Just ware -> ware
                  Nothing   -> errorWare n

factoriesDB :: [Factory]
factoriesDB = [ Factory "Пар Завод ракет \"Булава\" L"    [ Prod (findWErr "Батареи")                          ( -750 % (hms  0 10  0))
                                                          , Prod (findWErr "Руда")                             ( -125 % (hms  0 10  0))
                                                          , Prod (findWErr "Соевая мука")                      ( -100 % (hms  0 10  0))
                                                          , Prod (findWErr "Заградительная ракета \"Булава\"") (    5 % (hms  0 10  0))
                                                          ]

              , Factory "Пар Завод ракет \"Томагавк\" XL" [ Prod (findWErr "Батареи")                          (-3000 % (hms  0 20  0))
                                                          , Prod (findWErr "Руда")                             ( -500 % (hms  0 20  0))
                                                          , Prod (findWErr "Соевая мука")                      ( -400 % (hms  0 20  0))
                                                          , Prod (findWErr "Тяжелая ракета \"Томагавк\"")      (   30 % (hms  0 20  0))
                                                          ]

              , Factory "Пар Соевая фабрика XL"           [ Prod (findWErr "Батареи")                          ( -150 % (hms  0  1  0))
                                                          , Prod (findWErr "Соевые бобы")                      ( -120 % (hms  0  1  0))
                                                          , Prod (findWErr "Соевая мука")                      (   20 % (hms  0  1  0))
                                                          ]

              , Factory "Пар Соевая ферма XL"             [ Prod (findWErr "Батареи")                          ( -150 % (hms  0  1  0))
                                                          , Prod (findWErr "Соевые бобы")                      (  120 % (hms  0  1  0))
                                                          ]

              , Factory "Рудная шахта XL 1-15-30"         [ Prod (findWErr "Батареи")                          ( -180 % (hms  0  1 15))
                                                          , Prod (findWErr "Руда")                             (   30 % (hms  0  1 15))
                                                          ]

              , Factory "Рудная шахта XL 1-3-30"          [ Prod (findWErr "Батареи")                          ( -180 % (hms  0  1  3))
                                                          , Prod (findWErr "Руда")                             (   30 % (hms  0  1  3))
                                                          ]

              , Factory "Кремниевая шахта XL 1-58-20"     [ Prod (findWErr "Батареи")                          ( -480 % (hms  0  1 58))
                                                          , Prod (findWErr "Кремниевые пластины")              (   20 % (hms  0  1 58))
                                                          ]
              ]

findFactory :: FactoryName -> Maybe Factory
findFactory n = find cond factoriesDB
  where cond (Factory f _) = f == n

findWFactory :: FactoryName -> Factory
findWFactory n = case (findFactory n) of
                      Just factory -> factory
                      Nothing      -> errorFactory n