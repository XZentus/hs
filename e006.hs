main :: IO ()
main = do
    let limit = 100
        range = [1 .. limit]
        f = (\(s, p) -> (s, p*p, p*p - s)) . foldl (\(s, p) v -> (s + v * v, p + v)) (0, 0)
    print $ f range