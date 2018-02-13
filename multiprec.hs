import Data.Number.CReal
import Data.Number.BigFloat

type PrecPlus100 e = PrecPlus20  (PrecPlus20  (PrecPlus20  (PrecPlus20  (PrecPlus20  e))))
type PrecPlus500 e = PrecPlus100 (PrecPlus100 (PrecPlus100 (PrecPlus100 (PrecPlus100 e))))

x500 :: BigFloat (PrecPlus500 Eps1)
x500 = 100e300

print220 = ' ' : show result
  where
    x = 1 + 3 :: BigFloat (PrecPlus100 (PrecPlus100 (PrecPlus20 Eps1)))
    y = x * 5
    z =  y / 11
    result = z / pi

print500 = show result
  where
    x = 1 + 3 :: BigFloat (PrecPlus500 Eps1)
    y = x * 5
    z = y / 11
    result = z / pi

printCReal = flip showCReal result
  where
    x = 1 + 3
    y = x * 5
    z = y / 11
    result = z / pi

printDouble = result
  where
    x = 1 + 3
    y = x * 5
    z = y / 11
    result = z / pi

prn = do
    let s1 = print220
        s2 = printCReal 220
        df = getDiffs s1 s2
    putStrLn s1
    putStrLn s2
    putStrLn df

getDiffs :: String -> String -> String
getDiffs x y = p1 ++ p2
  where
    lx = length x
    ly = length y
    (mx, mn, lmn) = if lx > ly then (x, y, ly) else (y, x, lx)
    p1 = zipWith (\x y -> if x == y then ' ' else x) x y
    p2 = drop lmn mx
