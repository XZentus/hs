import Data.Foldable

genDiag :: Int -> Int -> Int
genDiag n x = (1 + x * 2) ^ 2 - n * x

main :: IO ()
main = do
    let raw_limit = 1001
        limit = raw_limit `div` 2
        result = foldl' (+) 1 $ concatMap (\f -> map f [1..limit]) $ map genDiag [0, 2 .. 6]
    print result
