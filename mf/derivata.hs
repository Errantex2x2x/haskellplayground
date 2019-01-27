data Fun a = X
           | E
           | Sin (Fun a)
           | Cos (Fun a)
           | Log (Fun a) (Fun a)
           | Con a 
           | Fun a :+: Fun a 
           | Fun a :-: Fun a
           | Fun a :*: Fun a 
           | Fun a :^: Int
           deriving Eq
           
infixr 2 :+:, :-:
infixr 3 :*:
infixl 4 :^:

instance (Show a) => Show (Fun a) where
  show = auxU
    where
      auxU X = "x"
      auxU (Con n) = show n
      auxU E = "e"
      auxU (Sin f) = "sin(" ++ auxU f ++ ")"
      auxU (Cos f) = "cos(" ++ auxU f ++ ")"
      auxU (Log b f) = "log_" ++ auxU b ++ "(" ++ auxU f ++ ")"
      auxU (f :+: g) = auxG 2 f ++ " + " ++ auxG 2 g
      auxU (f :-: g) = auxG 2 f ++ " - " ++ auxG 2 g
      auxU (f :*: g) = auxG 3 f ++ " * " ++ auxG 3 g
      auxU (f :^: n) = auxG 4 f ++ " ^ " ++ show n
  
      auxG p f@(_ :+: _) | p > 2 = "(" ++ auxU f ++ ")"
      auxG p f@(_ :-: _) | p > 2 = "(" ++ auxU f ++ ")"
      auxG p f@(_ :*: _) | p > 3 = "(" ++ auxU f ++ ")"
      auxG p f@(_ :*: _) | p > 4 = "(" ++ auxU f ++ ")"
      auxG _ f = auxU f

derive :: (Floating a) => Fun a -> Fun a
derive (Con _) = Con 0.0
derive X = Con 1.0
derive E = Con 0.0
derive (Sin f) = derive f :*: Cos f
derive (Cos f) = derive f :*: (Con 0.0 :-: Sin f)
derive (Log b f) = (f :*: (Log E b)) :^: -1
derive (f :+: g) = derive f :+: derive g
derive (f :-: g) = derive f :-: derive g
derive (f :*: g) = derive f :*: g :+: f :*: derive g
derive (f :^: n) = Con (fromIntegral n) :*: derive f :*: f :^: n-1

deriveS = (simplify.derive)

eval :: Fun Float -> Float -> Float
eval (Con n)   _ = n
eval X         v = v
eval E         _ = exp 1
eval (Sin f)   v = sin (eval f v)
eval (Cos f)   v = cos (eval f v)
eval (Log b f) v = logBase (eval b v) (eval f v)
eval (f :+: g) v = eval f v + eval g v
eval (f :-: g) v = eval f v - eval g v
eval (f :*: g) v = eval f v * eval g v
eval (f :^: n) v | n<0 = (1/eval f v) ^ (-n)
                 | otherwise = eval f v ^ n

simplify :: (Fractional a, Eq a) => Fun a -> Fun a
simplify (Sin f) = case simplify f of
  f -> Sin f
simplify (Cos f) = case simplify f of
  f -> Cos f
simplify (Log b f) = case (simplify b, simplify f) of
  (f, g) | f==g -> Con 1.0
  (b, Con 1.0)  -> Con 0.0
  (f, g) | f/=g -> Log f g
simplify (f :+: g) = case (simplify f, simplify g) of
  (Con n, Con m) -> Con (n+m)
  (Con 0.0, g)   -> g
  (f, Con 0.0)   -> f
  (f, g)         -> f :+: g
simplify (f :-: g) = case (simplify f, simplify g) of
  (Con n, Con m) -> Con (n-m)
  (f, Con 0.0)   -> f
  (f, g)         -> f :-: g
simplify (f :*: g) = case (simplify f, simplify g) of
  (Con n, Con m) -> Con (n*m)
  (_, Con 0.0)   -> Con 0.0
  (Con 0.0, _)   -> Con 0.0
  (f, Con 1.0)   -> f
  (Con 1.0, g)   -> g
  (f, g)         -> f :*: g
simplify (f :^: n) = case simplify f of
  Con m     -> Con (m^n)
  f | n==0  -> Con 1.0
  f | n==1  -> f
  (f :^: m) -> f :^: (m*n)
  f         -> f :^: n
simplify f = f

