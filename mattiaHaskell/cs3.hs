import Data.Char (ord)

-- arrotonda :: [Float] -> [Int]
-- arrotonda [] = []
-- arrotonda (x:xs) = round x : arrotonda xs

-- maiuscole :: [Char] -> [Char]
-- maiuscole [] = []
-- maiuscole (x:xs) = tuUpper x : maiuscole xs

-- mmap :: (a -> b) -> [a] -> [b]
-- mmap _ [] = []
-- mmap f (x:xs) = f x : mmap xs

-- arrotonda1 :: [Float] -> [Int]
-- arrotonda1 = map round

-- maiuscole1 :: [Char] -> [Char]
-- maiuscole1 = map toUpper

-- raddoppia :: Num a => [a] -> [a]
-- raddoppia = map (* 2)

-- eliminaSpazi :: String -> String
-- eliminaSpazi [] = []
-- eliminaSpazi (' ':xs) = eliminaSpazi xs
-- eliminaSpazi (x:xs) = x : eliminaSpazi xs

-- filter1 :: (a -> Bool) -> [a] -> [a]
-- filter1 _ [] = []
-- filter1 p (x:xs) | p x = x : filter1 p xs
--                  | otherwise = filter1 xs

-- eliminaSpazi1 :: String -> String
-- eliminaSpazi1 xs = filter1 (/= ' ')

-- qsort :: Ord a => [a] -> [a]
-- qsort [] = []
-- qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs) 

-- fatt n = foldr * 1 [1..n]

--Es3.1
eliminaUnicodeDispari :: String -> String
eliminaUnicodeDispari = filter (\x -> (ord x `mod` 2) == 0)

--Es3.2
convertiSpazi :: String -> String
convertiSpazi = map aux
  where
    aux ' ' = '_'
    aux c   = c

--Es3.3
maxLista :: Ord a => [a] -> a
maxLista ls = foldl max (head ls) ls

--Es3.4
sommaProdotti :: Num a => [(a,a)] -> a
sommaProdotti = foldr ((+) . (uncurry (*))) 0

--Es3.5
occorrenze :: Eq a => a -> [a] -> Int
occorrenze x ys = length (filter (==x) ys)
