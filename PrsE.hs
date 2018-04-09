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
    print $ runPrsEP (pure 42) 0 "ABCDEFG"
    puts_ $ "(0,Right (42,\"ABCDEFG\"))"
    newline
    let charEP c = satisfyEP (== c)
        anyEP = satisfyEP (const True)
        testP = (,) <$> anyEP <* charEP 'B' <*> anyEP
    print $ runPrsEP testP 0 "ABCDE"
    puts_ $ "(3,Right (('A','C'),\"DE\"))"
    newline
    print $ parseEP testP "BCDE"
    puts_ $ "Left \"pos 2: unexpected C\""
    newline
    print $ parseEP testP ""
    puts_ $ "Left \"pos 1: unexpected end of input\""
    newline
    print $ parseEP testP "B"
    puts_ $ "Left \"pos 2: unexpected end of input\""

    
newtype PrsEP a = PrsEP { runPrsEP :: Int -> String -> (Int, Either String (a, String)) }

instance Functor PrsEP where
    fmap f p = PrsEP (f' . succ) where
      f' pos s = case runPrsEP p pos s  of
                   (pos', Right (x, rest)) -> (pos', Right (f x, rest))
                   (pos', Left e)          -> (pos', Left e)

instance Applicative PrsEP where
    pure x = PrsEP $ \p s -> (p, Right (x, s))
    pf <*> pv = PrsEP f' where
      f' :: Int -> String -> (Int, Either String (b, String))
      f' pos s = do (pos1, val1) <- runPrsEP pf pos s
                    (p', s') <- val1
                    let (pos2, val2) = runPrsEP pv pos1 s'
                    (p'', s'') <- val2
                    return (pos2, (p' p'', s'') )

parseEP :: PrsEP a -> String -> Either String (a, String)
parseEP p  = snd . runPrsEP p 0

satisfyEP :: (Char -> Bool) -> PrsEP Char
satisfyEP f = PrsEP (f' . succ) where
    f' p []                 = (p, Left $ "pos " ++ show p ++ ": unexpected end of input")
    f' p (x:xs) | f x       = (p, Right (x, xs))
                | otherwise = (p, Left $ "pos " ++ show p ++ ": unexpected " ++ [x])
