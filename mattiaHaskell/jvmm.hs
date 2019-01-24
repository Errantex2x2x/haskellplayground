--Ambiente
data Value = F Float | I Int
  deriving (Show, Eq)
type Stack = [Value]
type Name = String
type Frame = [(Name, Value)]

--Frame spec ins
load :: Name -> Frame -> Value
load v fr = case lookup v fr of
  Just n -> n
  Nothing -> error (v++" not in scope")

store :: Name -> Value -> Frame -> Frame
store x v [] = [(x,v)]
store x v ((y,w):fr) | x==y      = (x,v):fr
                     | otherwise = (y,w):(store x v fr)

--Interprete
type Code = [Instruction]
data Instruction = PUSH Value
                 | LOAD Name
                 | STORE Name
                 | OP (Value -> Value -> Value)
                 | UOP (Value -> Value)
                 | IF (Value -> Value -> Bool) Code
                 | DUP
                 | SWAP
                 | POP
                 | NOP
                 | RETURN

aux :: Frame -> Stack -> Code -> Value
aux _  (v:_)    (RETURN:_)     = v 
aux fr vs       (PUSH v:is)    = aux fr (v:vs) is
aux fr vs       (LOAD x:is)    = aux fr (load x fr:vs) is
aux fr (v:vs)   (STORE x:is)   = aux (store x v fr) vs is
aux fr (w:v:vs) (OP f:is)      = aux fr (f w v:vs) is
aux fr (v:vs)   (UOP f:is)     = aux fr (f v:vs) is
aux fr (w:v:vs) (IF p is:is')  | p w v==True = aux fr vs is
                               | otherwise   = aux fr vs is'
aux fr (v:vs)   (DUP:is)       = aux fr (v:v:vs) is
aux fr (w:v:vs) (SWAP:is)      = aux fr (v:w:vs) is
aux fr (v:vs)   (POP:is)       = aux fr vs is
aux fr vs       (NOP:is)       = aux fr vs is

--Starter
run :: Code -> Value
run = aux [] []

--Tests
testDup :: Bool
testDup = case run init of
  I 1 -> True
  _   -> False
  where
    (===) (I a) (I b) = a==b
    init = PUSH (I 1) :
           DUP :
           IF (===) good :
           PUSH (I 0) : stop
    good = PUSH (I 1) : stop
    stop = RETURN : []

testPopSwap :: Bool
testPopSwap = case run init of
  I 1 -> True
  _   -> False
  where
    init = PUSH (I 0) :
           PUSH (I 1) :
           SWAP :
           POP :
           RETURN : []

--euclid MCD
mcd :: Int -> Int -> Int
mcd a b = case run init of I n -> n
  where
    n = "x"
    m = "y"
    (===) (I a) (I b) = a==b
    sub   (I a) (I b) = I (a-b)
    ge    (I a) (I b) = a>=b
    init = PUSH (I a) :
           STORE n :
           PUSH (I b) :
           STORE m : swap
    swap = LOAD m :
           LOAD n :
           IF (ge) loop :
           LOAD n :
           LOAD m :
           STORE n :
           STORE m : loop
    loop = LOAD m :
           PUSH (I 0) :
           IF (===) stop :
           LOAD m :
           LOAD n :
           OP (sub) :
           STORE n : swap    
    stop = LOAD n :
           RETURN : []
     
--stirling factorial approximation
stirling :: Float -> Float
stirling n = case run init of F y -> y
  where
    fmul (F a) (F b) = F (a*b)
    fsqrt (F a) = F (a**(1/2))
    fdiv (F a) (F b) = F (a/b)
    fexp (F b) (F e) = F (b**e)
    fpi = F 3.14
    fe = F 2.71828
    init = PUSH (F 2) :
           PUSH fpi :
           PUSH (F n) : 
           OP (fmul) :
           OP (fmul) :
           UOP (fsqrt) : -- sqrt(2 * pi * n)
           PUSH (F n) :
           PUSH fe :
           PUSH (F n) :
           OP (fdiv) : -- n/e
           OP (fexp) : -- (n/e)**n
           OP (fmul) : -- result
           RETURN : []
