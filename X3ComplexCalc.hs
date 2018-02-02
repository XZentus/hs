import qualified Data.Map.Strict as M
import Data.List
import Text.Printf

type FactoryName = String
type CycleLength = Double
type WareName = String
type Prices = (Double, Double, Double)
type Complex = [FactoryName]

data FactoryParams = FP CycleLength [(WareName, Double)]
    deriving Show

data WType = Raw | Inter | Final
    deriving (Eq, Show)

hms h m s = h * 60 * 60 + m * 60 + s
h1 = hms 1 0 0
m1 = hms 0 1 0
s1 = hms 0 0 1

wn <\> m = M.findWithDefault (999999999, 999999999, 999999999) wn m
wf </> m = M.findWithDefault (FP h1 [("Error: not found: " ++ wf, -999999999)]) wf m

waresDB :: M.Map WareName Prices
waresDB = M.fromList [ mw "Energy Cells"                         12    16    20
                     , mw "Ore"                                  50   128   206
                     , mw "Silicon Wafers"                      232   504   776
                     , mw "Tomahawk Heavy Missile"            20888 22460 24032
                     , mw "Flail Barrage Missile"             26954 33692 40430
                     , mw "Soja Husk"                           204   364   524
                     , mw "Soja Beans"                           14    28    42
                     ]
    where mw n d1 d2 d3 = (n, (d1, d2, d3))

factoriesDB :: M.Map FactoryName FactoryParams
factoriesDB = M.fromList [ mf "Par Tomahawk Missile Manufacturing Plant XL" (hms  0 20  0) [("Energy Cells",          -3000)
                                                                                           ,("Ore",                    -500)
                                                                                           ,("Soja Husk",              -400)
                                                                                           ,("Tomahawk Heavy Missile",   30)
                                                                                           ]
                         , mf "Par Flail Missile Production Facility L"     (hms  0 10  0) [("Energy Cells",           -750)
                                                                                           ,("Ore",                    -125)
                                                                                           ,("Soja Husk",              -100)
                                                                                           ,("Flail Barrage Missile",     5)
                                                                                           ]
                         , mf "Par Soyery XL"                               m1             [("Energy Cells",           -150)
                                                                                           ,("Soja Beans",             -120)
                                                                                           ,("Soja Husk",                20)
                                                                                           ]
                         , mf "Par Soyfarm XL"                              m1             [("Energy Cells",           -150)
                                                                                           ,("Soja Beans",              120)
                                                                                           ]
                         , mf "Par Ore Mine XL 1-15-30"                     (hms  0  1 15) [("Energy Cells",           -180)
                                                                                           ,("Ore",                      30)
                                                                                           ]
                         , mf "Par Ore Mine XL 1-3-30"                      (hms  0  1  3) [("Energy Cells",           -180)
                                                                                           ,("Ore",                      30)
                                                                                           ]
                         , mf "Silicon Mine XL 1-58-20"                     (hms  0  1 58) [("Energy Cells",           -480)
                                                                                           ,("Silicon Wafers",           20)
                                                                                           ]
                         ]
    where mf n c ws = (n, FP c ws)

complex :: [FactoryParams]
complex = concatMap (uncurry replicate . (\(n, f) -> (n, f </> factoriesDB)))
    [ (,) 2 "Par Flail Missile Production Facility L"
    , (,) 1 "Par Tomahawk Missile Manufacturing Plant XL"
    , (,) 2 "Par Soyery XL"
    , (,) 2 "Par Soyfarm XL"
    , (,) 1 "Par Ore Mine XL 1-15-30"
    , (,) 1 "Par Ore Mine XL 1-3-30"
    , (,) 1 "Silicon Mine XL 1-58-20"
    ]

getWT x = if x > 0 then Final else Raw

wt1 <> wt2 = if wt1 == wt2 then wt1 else Inter

calcComplex :: [FactoryParams] -> M.Map WareName (WType, Double)
calcComplex fs = cc fs M.empty
  where cc [] m = m
        cc ((FP l wares):ws) m = cc ws $ foldl' merge m wares
          where  merge mp (wn, n) = case M.lookup wn m of
                                         Just (wt, usn) -> M.insert wn (wt <> (getWT n), usn + n/l) mp
                                         Nothing        -> M.insert wn (       getWT n,        n/l) mp

printBalance :: M.Map WareName (WType, Double) -> Double -> IO ()
printBalance m l = do
    sumsr <- print' raw (0,0,0)
    putStrLn "------------------------------------------------------\n"
    sumsi <- print' inter (0,0,0)
    putStrLn "------------------------------------------------------\n"
    sumsf <- print' final (0,0,0)
    putStrLn "------------------------------------------------------\n" 
    printSum $ sumsr <+> sumsi <+> sumsf
  where
    (s1, s2, s3) <+> (d1, d2, d3) = (s1 + d1, s2 + d2, s3 + d3)
    raw   = M.toList $ M.filter (\(t, _) -> Raw   == t) m
    inter = M.toList $ M.filter (\(t, _) -> Inter == t) m
    final = M.toList $ M.filter (\(t, _) -> Final == t) m
    print' :: [(WareName, (WType, Double))] -> (Double, Double, Double) -> IO (Double, Double, Double)
    print' [] s = return s
    print' ((wn, (wt, d)):ws) ss = do
        putStr $ wn ++ ":\n\t"
        printf "\t%12.2f\n" (d * l)
        if d > 0
        then printf "\t%12.2f\t%12.2f\t%12.2f\n" (pmin * d * l) (pmed * d * l) (pmax * d * l)
        else printf "\t%12.2f\t%12.2f\t%12.2f\n" (pmax * d * l) (pmed * d * l) (pmin * d * l)
        print' ws (smin', smed', smax')
      where
        prices@(pmin,pmed,pmax) = wn <\> waresDB
        (s1, s2, s3) <>> (d1, d2, d3) = (s1 + d1 * d * l, s2 + d2 * d * l, s3 + d3 * d * l)
        (s1, s2, s3) <<> (d1, d2, d3) = (s1 + d3 * d * l, s2 + d2 * d * l, s3 + d1 * d * l)
        (smin', smed', smax') = if wt == Final then ss <>> prices else ss <<> prices
    printSum :: (Double, Double, Double) -> IO () 
    printSum (smin, smed, smax) = do
        putStrLn "------------------------------------------------------\n"
        printf "\t%12.2f\t%12.2f\t%12.2f\n" smin smed smax
