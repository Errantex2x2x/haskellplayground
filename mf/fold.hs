myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl _ x [] = x
myfoldl op x ys = (myfoldl op x (drop_last ys)) `op` (last ys)
  where
    last = head.reverse
    drop_last = reverse.(drop 1).reverse 

myfoldl_dual _ x [] = x
myfoldl_dual op x ys = foldr op1 x (reverse ys)
  where
    op1 y x = x `op` y

--myfoldr :: (b -> a -> a) -> a -> [b] -> a
myfoldr _ x [] = x
myfoldr op x (y:ys) = y `op` (myfoldl op x ys)
