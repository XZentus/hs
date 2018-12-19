import Data.List (maximumBy)

import Control.Monad
import Control.Monad.State

import qualified Data.Map.Strict as M

collatz :: Int -> Int
collatz = c 1
  where
    c n 1 = n
    c n x | even x    = c (n + 1) $ x `div` 2
          | otherwise = c (n + 1) $ 3 * x + 1

collatzStep x | even x    = x `div` 2
              | otherwise = 3 * x + 1

printCollatz :: Int -> IO ()
printCollatz 1 = putStrLn "1"
printCollatz x = do
    putStr (show x ++ " -> ")
    printCollatz $ collatzStep x
          
solve1 :: Int -> Int
solve1 limit = maximumBy ((. collatz) . compare . collatz) [2..limit]

baseState :: M.Map Int Int
baseState = M.fromList [(1, 1)
                       ,(2, 2)
                       ,(4, 3)
                       ,(8, 4)
                       ]

collatzState :: Int -> State (M.Map Int Int) Int
collatzState n = do
    h <- get
    case M.lookup n h of
        Just x  -> return x
        Nothing -> do
            let n' = collatzStep n
            res <- collatzState n'
            let r1 = res + 1
            h <- get
            put $ M.insert n r1 h
            return r1

solve2 :: Int -> (Int, Int)
solve2 limit = M.foldlWithKey' f (0, 0) db
  where
    db = execState (sequence_ . map collatzState $ [1..limit]) baseState
    f r@(_, cr) k b | cr > b    = r
                    | otherwise = (k, b)
                    
main :: IO ()
main = print $ solve2 1000000