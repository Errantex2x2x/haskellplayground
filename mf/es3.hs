intervallo :: Int -> Int -> [Int]
intervallo m n | m == n    = [m]
               | otherwise = m : intervallo (m+1) n

intervallo1 :: (Ord a, Enum a) => a -> a -> [a]
intervallo1 m n | m == n    = [m]
               | otherwise = m : intervallo (Succ m) n

divisori :: Int -> [Int]
divisori n = aux n n
  where
    aux 1 _ = [1]
    aux m n | n `mod` m == 0 = aux (m-1) n ++ [m]
            | otherwise      = aux (m-1) n

length1 :: [a] -> Int
length1 [] = 0
length1 (x:xs) = 1 + length xs

append [] ys = ys
append (x:xs) ys = x : append xs ys 

head1 (x:xs) = x

tail1 (x:xs) = xs

reverse1 (x:xs) = (reverse1 xs) ++ [x]

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = x ++ (concat xs)

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ [] = False
elem1 n (x:xs) = n==x || elem1 n xs

at1 :: [a] -> Int -> a
at1 (x:xs) 0 = x
at1 (x:xs) i = at1 xs (i-1)

take1 :: Int -> [a] -> [a]
take1 0 _ = []
take1 n (x:xs) = x : (take (n-1) xs)

drop1 :: Int -> [a] -> [a]
drop1 0 xs = xs
drop1 n (x:xs) = drop (n-1) xs

null1 :: [a] -> Bool
null1 [] = True
null1 _ = False

zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] _ = []
zip1 _ [] = []
zip1 (x:xs) (y:ys) = (x,y) : zip1 xs ys

unzip1 :: [(a,b)] -> ([a], [b])
unzip1 [] = ([], [])
unzip1 (x:xs) = (fst x : fst i, snd x : snd i) 
  where i=unzip1 xs

ordinata :: Ord a => [a] -> Bool
ordinata [] = True
ordinata [x] = True
ordinata (x:xs) = (x<= head xs) && ordinata xs
