import JVMM

mcd_hs :: Int -> Int -> Int
mcd_hs n m | m==0      = n
           | n<m       = mcd_hs m n
           | otherwise = mcd_hs (n-m) m

mcd :: Int -> Int -> Int
mcd x y = run init
  where
    m = "m"
    n = "n"
    init = PUSH x :
           STORE n :
           PUSH y :
           STORE m :
           loop
    loop = PUSH 0 :
           LOAD m :
           IF (==) stop :
           LOAD m :
           LOAD n :
           IF (<) swap :
           LOAD m :
           LOAD n :
           OP (-) :
           STORE n :
           loop
    swap = LOAD m :
           LOAD n :
           STORE m :
           STORE n :
           loop
    stop = LOAD n :
           RETURN : []             

