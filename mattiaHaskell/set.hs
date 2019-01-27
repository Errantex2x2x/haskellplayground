my_union :: Ord a => [a] -> [a] -> [a]
my_union [] (x:xs) = x:xs
my_union (x:xs) [] = x:xs
my_union [] [] = []
my_union (x:xs) (y:ys) | x==y = x : (my_union xs ys)
                       | x>y  = y : (my_union (x:xs) ys)
                       | x<y  = x : (my_union xs (y:ys))

my_intersection :: Ord a => [a] -> [a] -> [a]
my_intersection [] _ = []
my_intersection _ [] = []
my_intersection (x:xs) (y:ys) | x==y = x : (my_intersection xs ys)
                              | x>y  = my_intersection (x:xs) ys
                              | x<y  = my_intersection xs (y:ys)

my_difference :: Ord a => [a] -> [a] -> [a]
my_difference [] (x:xs) = []
my_difference (x:xs) [] = x:xs
my_difference [] [] = []
my_difference (x:xs) (y:ys) | x==y = my_difference xs ys
                            | x>y  = my_difference (x:xs) ys
                            | x<y  = x : (my_difference xs (y:ys))
