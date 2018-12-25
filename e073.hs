import Data.Ratio

nextRatio1d3 :: Int -> Ratio Int
nextRatio1d3 d = (1 + d `div` 3) % d

range1d3_1d2 :: Int -> [ Ratio Int ]
range1d3_1d2 d = let nr = nextRatio1d3 d
                 in [nr, nr + 1%d .. 99999%200000]

uniqueRange :: Int -> [ Ratio Int ]
uniqueRange d = filter ((== d) . denominator) $ range1d3_1d2 d

main :: IO ()
main = print $ sum $ map (length . uniqueRange) [3..12000]