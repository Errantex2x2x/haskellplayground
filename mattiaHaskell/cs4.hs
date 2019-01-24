match :: Eq a => [a] -> [a] -> Bool
match xs ys = foldr ((||).(\(x,y) -> x==y)) False (zip xs ys)

adiacenti :: Eq a => [a] -> Bool
adiacenti xs = match xs (tail xs)

polinomio :: [Float] -> Float -> Float
polinomio as x = sum terms
  where
    terms = map (\c -> (fst c)*(x**(snd c))) couples
    couples = zip as [0..]

poli2 :: [Float] -> Float -> Float
poli2 as x = foldr (\a c -> a+(x*c)) 0 as

perfetto :: Int -> Bool
perfetto n = n == (sum (divisori n))
  where
    divisori n = filter (\x -> (n `mod` x) == 0) [1..n-1]

ordinata :: Ord a => [a] -> Bool
ordinata xs = foldr ((&&).(\(x,y) -> x<=y)) True (zip xs (tail xs))

-- match :: Eq a => [a] -> [a] -> Bool
-- match xs ys = any (uncurry (==)) (zip xs ys)

-- polinomio :: [Float] -> Float -> Float
-- polinomio cs x = sum (map (uncurry (*)) (zip cs (map (x ^) [0..])))

-- perfetto :: Int -> Bool
-- perfetto n = n == sum (filter ((== 0) . (n `mod`)) [1 .. n - 1])

-- ordinata :: Ord a => [a] -> Bool
-- ordinata xs = all (uncurry (<=)) (zip xs (tail xs))

-- adiacenti :: Eq a => [a] -> Bool
-- adiacenti xs = any (uncurry (==)) (zip xs (tail xs))
