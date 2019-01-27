type Name = String
data Term = Var Name | Lam Name Term | App Term Term

test0 :: Term
test0 = (Lam "x" (App (Var "x") (Var "y")))

test1 :: Term
test1 = Lam "x" $ Lam "y" $ App (Lam "f" (Var "x")) (Var "a") 

test2 :: Term
test2 = Lam "x" $ App (Var "x") (Lam "f" (Var "a"))

bv :: Term -> [Name]
bv (Var x)      = [x]
bv (Lam n t)    = [x | x <- (bv t), x == n]   
bv (App t1 t2)  = bv t1 ++ bv t2

fv :: Term -> [Name]
fv (Var x)      = [x]
fv (Lam n t)    = [x | x <- (fv t), x /= n]   
fv (App t1 t2)  = fv t1 ++ fv t2

rmDup :: Eq a => [a] -> [a]
rmDup (x:xs) = x : [y | y <- rmDup xs, y /= x]
rmDup xs = xs

checkEta :: Name -> Term -> Bool
checkEta n (App t1 (Var x)) | n == x = False
                            | otherwise = True

checkBeta :: Term -> Term -> Bool
checkBeta (Lam _ _) _ = False
checkBeta _ _         = True

normal :: Term -> Bool
normal (Var x)         = True
normal (Lam n (Var _)) = True
normal (Lam n t)       = (notElem n (fv t)) && (checkEta n t) && (normal t) 
normal (App t1 t2)     = (checkBeta t1 t2) && (normal t1) && (normal t2)



