import Control.Monad.State

import qualified Data.Map.Strict as M

fib :: Int -> State (M.Map Int Integer) Integer
fib n | n < 2     = return 1
      | otherwise = do
          h <- get
          case M.lookup n h of
           Just i  -> return i
           Nothing -> do
             ns <- sequence $ map (\i -> fib $ n - i) [2, 1]
             let next = sum ns
             h <- get
             put $ M.insert n next h
             return next

main :: IO ()
main = do
  print $ runState (fib 5) M.empty
