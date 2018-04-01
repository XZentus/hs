{-# LANGUAGE TypeOperators #-}

import Data.Monoid

infixr 9 |.|
newtype (|.|) f g a = Cmps { getCmps :: f (g a) }  deriving (Eq,Show) 

mkEndo :: Foldable t => t (a -> a) -> Endo a
mkEndo = foldMap Endo

instance Foldable (f |.| g) where
--  foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f (Cmps c) = (mappend . c) mempty f
    --foldMap = undefined
--  foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
    --foldr f ini (Cmps c) = f (Endo . Endo $ c) ini
    
--f fun (Cmps x) = fmap fun x
    
test :: IO ()
test = do
    putStrLn $ "maximum $ Cmps [Nothing, Just 2, Just 3]"
    print    $  maximum $ Cmps [Nothing, Just 2, Just 3]
    putStrLn $ "length $ Cmps [[1,2], [], [3,4,5,6,7]]"
    print    $  length $ Cmps [[1,2], [], [3,4,5,6,7]]
