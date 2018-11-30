import Data.Ratio

main :: IO ()
main = print $ maximum $ filter (< 3%7) $ map (\x -> (floor $ 3*x % 7) % x) $ [2..1000000]
