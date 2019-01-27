import Data.Char (intToDigit)

bisestile :: Integer -> Bool
bisestile n = ((n `mod` 4) == 0 && (n `mod` 100) /= 0) || (n `mod` 400) == 0

cifra :: Int -> Char
cifra = intToDigit

striling :: Int -> Double
striling n = let m = fromIntegral n in
  sqrt (m * 2 * pi) * (m / exp 1) ** m 

fattoriale :: Integer -> Integer
fattoriale n | n > 0  = n * fattoriale(n-1)
             | n == 0 = 1
             | n < 0  = error "not positive fact"

fibo :: Int -> Int
fibo 0 = 0
fibo 1 = 1
fibo n = fibo(n-1) + fibo(n-2)

bits :: Integer -> Int
bits n | n == 0       = 0
       | mod n 2 == 1 = 1 + bits(div n 2)
       | otherwise    = bits(div n 2)

--- ruota 1234 = 4123
ruota :: Int -> Int
ruota n = let m = mod n 10 in
  (m * 10^(digits n -1)) + (div n 10)
  where
    digits 0 = 0
    digits n = 1 + digits(div n 10) 

fuga :: Float -> Float -> Float
fuga m r = sqrt (2 * 9.8 * m / r)

potenza :: Int -> Int -> Int
potenza m 0 = 1
potenza m n = m * potenza m (n-1)

base :: Int -> Int -> String
base 0 _ = "0"
base n b = base (n `div` b) b ++ [cifra (mod n b)]
