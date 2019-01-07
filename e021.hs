import qualified Data.Vector.Unboxed as V

import Data.List

divsSum :: Int -> V.Vector Int
divsSum to = V.accum (+) initVec divsAll
  where
    initVec   = V.replicate (to + 1) 1
    divsAll   = concatMap divsGen [2 .. to `div` 2]
    divsGen d = let d2 = d * 2
                in map (flip (,) d) [d2, d2 + d .. to]

isAmicable :: V.Vector Int -> Int -> Bool
isAmicable v i = i' < lim && i' /= i && i'' == i
  where
    i'  = v V.! i
    i'' = v V.! i'
    lim = V.length v

amicablesSum :: V.Vector Int -> Int
amicablesSum v = foldl' f 0 [2 .. V.length v - 1]
  where
    f s i = if isAmicable v i then s + i else s
    
main :: IO ()
main = do
    let sums = divsSum 10000
    print $ amicablesSum sums