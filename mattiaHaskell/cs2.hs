--Case study 2
fattoriale :: Int -> Int
fattoriale = aux 1
  where
    aux k n | n==0 = k
            | n>0  = aux (k*n) (n-1)

fattoriale2 :: Int -> Int
fattoriale2 n = n * fattoriale2 (n-1)

bits :: Int -> Int
bits = aux 0
  where
    aux k n | n==0 = k
            | n>0  = aux (k+(n `mod` 2)) (div n 2)

euclide :: Int -> Int -> Int
euclide 0 n = n
euclide m n | m<n       = euclide n m
            | otherwise = euclide (m-n) n
