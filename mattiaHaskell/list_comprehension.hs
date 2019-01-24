mymap :: (a -> b) -> [a] -> [b]
mymap f xs = [ f x | x <- xs ]

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f xs = [ x | x <- xs, f x ]

primo :: Integral a => a -> Bool
primo n = foldr (\x y -> (x /= 0) && y) True [ n `mod` x | x <- [2..n-1] ]

terne :: Integral a => a -> [(a, a, a)]
terne n = [(a, b, c) | a <- xs, b <- xs, c <- xs, a*a + b*b == c*c, a<=b, b<=c ]
  where
    xs = [1..n]
