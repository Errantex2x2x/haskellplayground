sum_last :: (Num a,Eq a) => [a] -> Bool
sum_last = aux 0
  where
    aux n [x]    = n == x
    aux n (x:xs) = aux (n+x) xs

sum_last1 :: (Num a,Eq a) => [a] -> Bool
sum_last1 xs = (foldl (+) 0 (take_head xs)) == (last xs)
  where
    take_head = tail.reverse

take_max_length :: [[a]] -> [[a]]
take_max_length xs = filter ((== (max_sublist_length xs)).length) xs
  where
    max_sublist_length = maximum.(map length)

bilanciata :: String -> Bool
bilanciata = aux 0
  where
    aux n []     = n==0
    aux n (x:xs) | x=='('    = aux (n+1) xs 
                 | x==')'    = n>0 && aux (n-1) xs 
                 | otherwise = aux n xs

isSubList :: Eq a => [a] -> [a] -> Bool
isSubList [] _ = True
isSubList (x:xs) [] = False
isSubList (x:xs) (y:ys) | x==y      = isSubList xs ys
                        | otherwise = isSubList (x:xs) ys

allSubLists :: [a] -> [[a]]
allSubLists [] = [[]]
allSubLists (x:xs) = (map (x:) asl_xs) ++ asl_xs
  where
    asl_xs = allSubLists xs

my_map :: (a -> b) -> [a] -> [b]
my_map f = foldr ((:).f) []
--foldr f x (l:ls) = f l (foldr x ls)

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter f = foldr ((++).(\x -> if (f x) then [x] else [])) []

