solve :: Num a => [a] -> a -> a
solve xs n = aux xs n 0
    where 
        aux [] _ _     = 0
        aux (x:xs) n i = x*(n^i) + aux xs n (i+1)

solve2 :: Num a => [a] -> a -> a
solve2 xs n = foldr (+) 0 $ map (uncurry (\y i -> y*(n^i))) $ zip xs [0..]

solve3 :: Num a => [a] -> a -> a
solve3 xs n = foldr (+) 0 $ zipWith (\y i -> y*(n^i)) xs [0..]

solveEta :: Num a => a -> [a] -> a
solveEta n = sum . zipWith (\i y -> y*(n^i)) [0..]


-- solve2 xs n = foldr (+) 0 [ x | (y, i) <- zip xs [0..], x == y*(n^i)]wit

