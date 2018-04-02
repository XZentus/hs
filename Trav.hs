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

test :: IO ()
test =
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
main = test

unf4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
unf4 f (a, b, c, d) = f a b c d

instance Foldable Tree where
    foldr f ini Nil = ini
    foldr f ini (Branch l x r) = (foldr f (f x (foldr f ini r)) l)
