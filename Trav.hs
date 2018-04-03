module Main where

import Data.Foldable
import Control.Applicative

data Tree a = Nil | Branch (Tree a) a (Tree a)   deriving (Eq, Show)

sequenceA2list :: (Foldable t, Applicative f) => t (f a) -> f [a]
sequenceA2list = foldr (\x y -> (:) <$> x <*> y) $ pure []

traverse2list :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f [b]
traverse2list f cont = foldr (\x y -> (:) <$> (f x) <*> y) (pure []) cont

printTest :: (Eq a, Show a) => String -> String -> a -> a -> IO ()
printTest s1 s2 a1 a2 = do
    putStrLn s1
    print a1
    putStrLn s2
    print $ a1 == a2

testtraverse2list, testTriple :: IO ()
testtraverse2list =
    traverse_ (unf4 printTest) [( "traverse2list (\\x -> [x+10,x+20]) [1,2,3]"
                                , "[[11,12,13],[11,12,23],[11,22,13],[11,22,23],[21,12,13],[21,12,23],[21,22,13],[21,22,23]]"
                                ,  traverse2list (\x -> [x+10,x+20]) [1,2,3]
                                ,  [[11,12,13],[11,12,23],[11,22,13],[11,22,23],[21,12,13],[21,12,23],[21,22,13],[21,22,23]]
                                )
                               ,( "traverse2list (\\x -> [x+10,x+20]) $ Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)"
                                , "[[12,11,13],[12,11,23],[12,21,13],[12,21,23],[22,11,13],[22,11,23],[22,21,13],[22,21,23]]"
                                ,  traverse2list (\x -> [x+10,x+20]) $ Branch (Branch Nil 1 Nil) 2 (Branch Nil 3 Nil)
                                ,  [[12,11,13],[12,11,23],[12,21,13],[12,21,23],[22,11,13],[22,11,23],[22,21,13],[22,21,23]]
                                )
                               ]

testTriple = do
    (unf4 printTest) ( "foldl (++) \"!!\" (Tr \"ab\" \"cd\" \"efg\")"
                                , "!!abcdefg"
                                ,  foldl (++) "!!" (Tr "ab" "cd" "efg")
                                , "!!abcdefg"
                                )
    (unf4 printTest) ( "traverse (\\x -> if x>10 then Right x else Left x) (Tr 12 14 16)"
                                , "Right (Tr 12 14 16)"
                                ,  traverse (\x -> if x>10 then Right x else Left x) (Tr 12 14 16)
                                ,  Right (Tr 12 14 16)
                                )
    (unf4 printTest) ( "traverse (\\x -> if x>10 then Right x else Left x) (Tr 12 8 4)"
                                , "Left 8"
                                ,  traverse (\x -> if x>10 then Right x else Left x) (Tr 12 8 4)
                                ,  Left 8
                                )
    (unf4 printTest) ( "sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))"
                                , "Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)"
                                ,  sequenceA (Tr (Tr 1 2 3) (Tr 4 5 6) (Tr 7 8 9))
                                ,  Tr (Tr 1 4 7) (Tr 2 5 8) (Tr 3 6 9)
                                )

data Triple a = Tr a a a  deriving (Eq,Show)

instance Functor Triple where
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Foldable Triple where
--  foldr :: (a -> b -> b) -> b -> t a -> b 
    foldr f ini (Tr x y z) = f x $ f y $ f z ini

instance Applicative Triple where
    pure x = Tr x x x
--  (<*>) :: f (a -> b) -> f a -> f b
    (Tr f1 f2 f3) <*> (Tr v1 v2 v3) = Tr (f1 v1) (f2 v2) (f3 v3)

instance Traversable Triple where
--  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f (Tr x y z) = Tr <$> (f x) <*> (f y) <*> (f z)
--  sequenceA :: Applicative f => t (f a) -> f (t a) 

main = testTriple

unf4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
unf4 f (a, b, c, d) = f a b c d

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = (foldr f (f x (foldr f ini r)) l)
