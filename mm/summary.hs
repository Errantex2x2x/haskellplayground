type Name = String
data Term = Var Name | Lam Name Term | App Term Term

test :: Term
test = Lam "x" $ Lam "y" $ App (Lam "f" (Var "x")) (Var "a") 

test2 :: Term
test2 = Lam "x" $ App (Var "s") (Lam "f" (Var "a")) 

rmdup :: Eq a => [a] -> [a]
rmdup (x:xs) = x : (rmdup [a | a <- xs, a /= x])
rmdup xs = xs

fv :: Term -> [Name]
fv (Var x) = [x]
fv (Lam x y) = filter (\s -> s /= x) (fv y)
fv (App x y) = rmdup $ fv x ++ fv y

bv :: Term -> [Name]
bv (Var x) = [x]
bv (Lam x y) = filter (\s -> s == x) (fv y)
bv (App x y) = rmdup $ fv x ++ fv y

normal :: Term -> Bool
normal (Var x) = True
normal (Lam x y) = ((length [a | a<-fv y, a == x] /= 1) ||
                    not (islast y x)) && normal y
                    where
                        islast (App a (Var b)) y = b == y
                        islast _ _ = False
normal (App x y) = (case x of 
                         Lam a b -> False
                         otherwise -> True
                    )
                    && normal x && normal y