newtype PrsE a = PrsE { runPrsE :: String -> Either String (a, String) }

satisfyE :: (Char -> Bool) -> PrsE Char
satisfyE f = PrsE p where
    p [] = Left "unexpected end of input"
    p (c:cs) | f c       = Right (c, cs)
             | otherwise = Left $ "unexpected " ++ [c]

charE :: Char -> PrsE Char
charE c = satisfyE (== c)

instance Functor PrsE where
  fmap f p = PrsE f'
    where f' s = case runPrsE p s of
                      Right (a, rest) -> Right (f a, rest)
                      Left x          -> Left x
             
instance Applicative PrsE where
  pure a = PrsE $ \s -> Right (a, s)
--(<*>) :: Applicative f => f (a -> b) -> f a -> f b
  pf <*> pv = PrsE f where
    f s = do (p', s')   <- runPrsE pf s
             (p'', s'') <- runPrsE pv s'
             return $ (p' p'', s'')

instance Monad PrsE where
--(>>=) :: forall a b. m a -> (a -> m b) -> m b 
  (>>=) f kleis = PrsE f' where
                  f' s = do (v',  s') <- runPrsE f s
                            result    <- runPrsE (kleis v') s'
                            return result

main = do
    print $ runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ABC"
    print $ (Right (('A','B'),"C") :: Either String ((Char, Char), String))
    print $ runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "ACD"
    print $ (Left "unexpected C" :: Either String ((Char, Char), String))
    print $ runPrsE (do {a <- charE 'A'; b <- charE 'B'; return (a,b)}) "BCD"
    print $ (Left "unexpected B" :: Either String ((Char, Char), String))
    
