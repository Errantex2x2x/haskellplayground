import Control.Monad.State.Lazy (State, get, put, return, evalState) 

type Name = String
data Term = Var Name | Lam Name Term | App Term Term
  deriving Eq

settify :: Eq a => [a] -> [a]
settify [] = []
settify (x:xs) | elem x (settify xs) = settify xs
               | otherwise = x : settify xs

fv :: Term -> [Name]
fv (Var n) = [n]
fv (Lam n t) = [e | e <- fv t, e/=n]
fv (App t1 t2) = settify ins
  where
    ins = fv t1 ++ fv t2

bv :: Term -> [Name]
bv (Var n) = []
bv (Lam n t) = n:bv t
bv (App t1 t2) = settify (bv t1 ++ bv t2)

-- rename :: Term -> Term
-- rename t = undefined
--   where
--     aux :: Term -> 

normal :: Term -> Bool
normal (Var n) = True
normal (Lam x t) = case t of
  App t1 t2 | t2==Var x -> False
  otherwise -> normal t
normal (App x y) = case x of
  Lam _ _ -> False
  otherwise -> normal x && normal y

data Tree a = Leaf | Branch a (Tree a) (Tree a)
bynarySearchTree :: Ord a => Tree a -> Maybe ((a,a),a,(a,a))
bynarySearchTree Leaf = Nothing
bynarySearchTree (Branch x t1 t2) = 
