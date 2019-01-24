somma :: Integer -> Integer
somma n = n * (n+1) `div` 2

pari :: Integer -> Bool
pari n = n `mod` 2 == 0

dispari :: Integer -> Bool
dispari = not . pari

assoluto n | n >= 0    = n
           | otherwise = -n
