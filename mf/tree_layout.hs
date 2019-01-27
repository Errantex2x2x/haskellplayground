import Control.Monad.State.Lazy (State, put, get, evalState)

data Tree a = Leaf | Branch a (Tree a) (Tree a)
  deriving Show

type X = Int
type Y = Int
type Pos = (X,Y)

layout :: Tree a -> Tree Pos
layout = fst.(aux 0 0)
  where
    aux :: X -> Y -> Tree b -> (Tree Pos, X)
    aux x y Leaf = (Branch (x,y) Leaf Leaf, x+1)
    aux x y (Branch _ t1 t2) = (Branch (x1,y) s1 s2, x2)
      where
        (s1, x1) = aux x (y-1) t1
        (s2, x2) = aux (x1+1) (y-1) t2

type Layout a = State X a

nextX :: Layout X
nextX = do x <- get
           put (x+1)
           return x

layoutM :: Tree a -> Tree Pos
layoutM t = evalState (aux 0 t) 0
  where
    aux :: Y -> Tree b -> Layout (Tree Pos)
    aux y Leaf = do x <- nextX
                    return (Branch (x,y) Leaf Leaf)
    aux y (Branch _ t1 t2) = do s1 <- aux (y-1) t1
                                x <- nextX
                                s2 <- aux (y-1) t2
                                return (Branch (x,y) s1 s2)

--Esercizi

type LeafCounter = State Int Int

leaves :: Tree Int -> Int
leaves t = evalState (aux t >> get) 0
  where
    aux :: Tree Int -> LeafCounter
    -- aux Leaf = do x <- nextX
    --               return x
    -- aux (Branch _ t1 t2) = do r1 <- aux t1
    --                           r2 <- aux t2
    --                           r  <- get
    --                           return r
    aux Leaf = nextX
    aux (Branch _ t1 t2) = aux t1 >> aux t2

renumber :: Tree a -> Tree Int
renumber t = evalState (aux t) 0
  where
    aux :: Tree a -> State Int (Tree Int)
    aux Leaf = return Leaf
    aux (Branch _ t1 t2) = do r1 <- aux t1
                              x <- nextX
                              r2 <- aux t2
                              return (Branch x r1 r2)

type LayoutStatePos a = State Pos a

nextX' :: LayoutStatePos Int
nextX' = do (x,y) <- get
            put (x+1,y)
            return x

decY :: LayoutStatePos ()
decY = do (x,y) <- get
          put (x,y-1)

incY :: LayoutStatePos ()
incY = do (x,y) <- get
          put (x,y+1)

layoutMM :: Tree a -> Tree Pos
layoutMM t = evalState (aux t) (0,0)
  where
    aux :: Tree a -> LayoutStatePos (Tree Pos)
    aux Leaf = do r <- get
                  return (Branch r Leaf Leaf)
    aux (Branch _ t1 t2) = do decY
                              r1 <- aux t1
                              x <- nextX'
                              r2 <- aux t2
                              incY
                              c <- get
                              return (Branch c r1 r2)

type Tmap a b = State ([(a,Int)],Int) b

remap :: Eq a => Tree a -> Tree Int
remap t = evalState (aux t) ([],0)
  where
    aux :: Eq a => Tree a -> Tmap a (Tree Int)
    aux Leaf = return Leaf
    aux (Branch v t1 t2) = do r1 <- aux t1
                              x <- findX v
                              r2 <- aux t2
                              return (Branch x r1 r2)

findX :: Eq a => a -> Tmap a Int
findX x = do (ls,y) <- get
             case lookup x ls of
               Just n -> put (ls,y+1) >> return n
               Nothing -> put ((x,y):ls,y+1) >> return y
