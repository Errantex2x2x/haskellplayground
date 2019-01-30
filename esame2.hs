solve :: Num a => [a] -> a
solve xs = aux xs 0
	where
		aux [] i     = 0
		aux (x:xs) i = x*(2^i) + aux xs (i+1)

solve1 :: Num a => [a] -> a
solve1 = sum . zipWith (\x y -> (2^x)*y) [0..]
