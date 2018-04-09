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

newline = putStrLn ""
puts_ = putStrLn

main = do
    let charEP c = satisfyEP (== c)
    print $ runPrsEP (charEP 'A') 0 "ABC"
    puts_ $ "(1,Right ('A',\"BC\"))"
    newline
    print $ runPrsEP (charEP 'A') 41 "BCD"
    puts_ $ "(42,Left \"pos 42: unexpected B\")"
    newline
    print $ runPrsEP (charEP 'A') 41 ""
    puts_ $ "(42,Left \"pos 42: unexpected end of input\")"
    newline
    print $ parseEP (charEP 'A') "ABC"
    puts_ $ "Right ('A',\"BC\")"
    newline
    print $ parseEP (charEP 'A') "BCD"
    puts_ $ "Left \"pos 1: unexpected B\""
    newline
    print $ parseEP (charEP 'A') ""
    puts_ $ "Left \"pos 1: unexpected end of input\""

    


newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP f = PrsEP f' where
    f' pos []                 = let p = pos + 1 in (p, Left $ "pos " ++ show p ++ ": unexpected end of input")
    f' pos (x:xs) | f x       = let p = pos + 1 in (p, Right (x, xs))
                  | otherwise = let p = pos + 1 in (p, Left $ "pos " ++ show p ++ ": unexpected " ++ [x])
