data Tree a = Leaf | Branch a (Tree a) (Tree a)

--Yes min max
data YesNo elem = Yes (elem) (elem) | No

instance Eq (YesNo elem) where
    (==) (Yes _ _) (Yes _ _) = True
    (==) No No               = True
    (==) _ _                 = False
    (/=) a b                 = not $ (==) a b

test :: Tree Int
test = Branch 5 (Branch 4 (Branch 3 Leaf Leaf) Leaf) (Branch 1 Leaf Leaf)

binSearch :: Ord a => Tree a -> a -> Bool
binSearch Leaf _                        = False
binSearch (Branch x xl xr) e | e == x   = True
                             | e < x    = binSearch xl e 
                             | e > x    = binSearch xr e

isBinSearchTree :: Ord a => Tree a -> Bool
isBinSearchTree Leaf = True
isBinSearchTree (Branch x xl xr) = aux (Branch x xl xr) /= No
    where
        aux (Branch x Leaf Leaf) = Yes x x 
        aux (Branch x Leaf xr)   = isMinGreater (aux xr) x --checks that the min of xs is > x         
        aux (Branch x xl Leaf)   = isMaxLess (aux xl) x    --checks that the max of xl is < x
        aux (Branch x xl xr)     = ynand (isMinGreater (aux xr) x) (isMaxLess (aux xl) x) --merge

ynand :: YesNo elem -> YesNo elem -> YesNo elem  
ynand No _ = No
ynand _ No = No
ynand (Yes min1 max1) (Yes min2 max2) = Yes min1 max2

isMinGreater :: Ord elem => YesNo elem -> elem -> YesNo elem
isMinGreater No _ = No 
isMinGreater (Yes min max) x | min >= x  = Yes x max
                             | otherwise = No

isMaxLess :: Ord elem => YesNo elem -> elem -> YesNo elem
isMaxLess No _ = No 
isMaxLess (Yes min max) x    | max <= x  = Yes min x
                             | otherwise = No


