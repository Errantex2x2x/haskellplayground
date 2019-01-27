data Tree a = Leaf | Branch a (Tree a) (Tree a)

instance Show a => Show (Tree a) where
  show t = "{" ++ aux t ++ "}"
    where
      aux Leaf = ""
      aux (Branch a t1 t2) = show a ++ show t1 ++ show t2

elements :: Tree a -> [a]
elements Leaf = []
elements (Branch a t1 t2) = a:(elements t1 ++ elements t2)

member :: Ord a => a -> Tree a -> Bool
member _ Leaf  = False
member x (Branch a t1 t2) | x==a = True
                          | x<a  = member x t1
                          | x>a  = member x t2

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Branch x Leaf Leaf
insert x t@(Branch a t1 t2) | x==a = t
                            | x<a  = Branch a (insert x t1) t2
                            | x>a  = Branch a t1 (insert x t2)

minT :: Ord a => Tree a -> a
minT (Branch a t1 t2) = case t1 of
  Leaf -> a
  otherwise -> minT t1

delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf = Leaf
delete x (Branch a t1 t2)
  | x<a  = Branch a (delete x t1) t2
  | x>a  = Branch a t1 (delete x t2)
  | x==a = case t2 of Leaf -> t1
                      otherwise -> Branch (minT t2) t1 (delete (minT t2) t2)
