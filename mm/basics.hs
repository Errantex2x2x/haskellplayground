--get elements at even position. No Prelude.
evenPositionElements :: [a]->[a]
evenPositionElements [] = []
evenPositionElements [x] = [x]
evenPositionElements (x:_:xs) = x :(evenPositionElements xs) 

--get elements at even position. Uses Prelude
evenPositionElements' :: [a]->[a]
evenPositionElements' x = map snd (filter (even . fst) (zip [0..] x)) 

--finds the number of all elements which are followed by a smaller value. No Prelude.
inversions:: Ord a => [a]->Int
inversions (x':x'':xs) | x' > x''  = 1 + (inversions (x'':xs))
                       | otherwise = inversions (x'':xs)
inversions _ = 0

--finds the number of all elements which are followed by a smaller value. Uses Prelude.
inversions':: Ord a => [a]->Int
inversions' xs = length (map fst (filter (minsec) (zip xs (tail xs))))
                 where minsec x = (fst x) > (snd x)

--finds the number of all elements which are followed by a smaller value. Uses Prelude.
inversions'':: Ord a => [a]->Int
inversions'' xs = length ((filter (uncurry (>)) (zip xs (tail xs))))


--TREES

data Tree a = Empty | Node a [Tree a]

instance Show a => Show (Tree a) where
    show Empty = "Empty"
    show (Node x ts) = show x ++ " " ++ show ts ++ " "

--flats a tree in a list, visiting in depth first
elements:: Tree a -> [a]
elements Empty = []
elements (Node x ts) = x : concat (map elements ts)
-- test with: 
-- elements (Node 1 [Node 2 [], Node 4 [Node 6 []], Node 888 [Empty]])


--Removes all Occurences of "Empty" in a tree
normalize:: Tree a -> Tree a
normalize Empty = Empty
normalize (Node x ts) = Node x (map normalize (filter notempty ts))
                      where notempty Empty = False
                            notempty _ = True 

-- test with: 
-- normalize (Node 1 [Node 2 [], Node 4 [Node 6 []], Node 888 [Empty]])

--Removes all Occurences of "Empty" in a tree
normalize':: Tree a -> Tree a
normalize' Empty = Empty
normalize' (Node x ts) = Node x (concat (map(remempty . normalize) ts))
                      where remempty Empty = []
                            remempty t = [t]
