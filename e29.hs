import Data.List

remDupsSorted :: Eq a => [a] -> [a]
remDupsSorted (a:b:rest) | a == b    =      remDupsSorted $ b:rest
                         | otherwise = a : (remDupsSorted $ b:rest)
remDupsSorted r = r

main :: IO ()
main = do
    let range = [2 .. 100]
        r =  sort [ a ^ b | a <- range, b <- range]
        rnub = remDupsSorted r
    print $ length rnub
