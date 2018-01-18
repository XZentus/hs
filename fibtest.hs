import qualified Data.Vector as V
import Data.Char

fastFib :: V.Vector Integer -> Int -> (V.Vector Integer, Integer)
fastFib vec i | vlen < 2  = fastFib (V.fromList [1, 1]) i
              | vlen < i  = let next = vec V.! (vlen - 1) + vec V.! (vlen - 2)
                            in fastFib (V.snoc vec next) i
              | otherwise = (vec, vec V.! (i - 1))
    where vlen = V.length vec

main :: IO ()
main = routine $ V.fromList [1, 1, 1, 2, 3, 5]

routine :: V.Vector Integer -> IO ()
routine v = do
    putStr "> "
    input <- getLine
    let isInteger = and $ map isDigit input
    if isInteger
    then do
        let x = read input
            (newVector, result) = fastFib v x
        print result
        routine newVector
    else return ()
