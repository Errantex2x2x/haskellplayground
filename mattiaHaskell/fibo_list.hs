fibo :: [Int]
fibo = 1:1:[n+m | n <- tail fibo, m <- [pred n fibo]]                     
  where
    pred :: Int -> [Int] -> Int
    pred n (x:xs) | head xs == n = x
                  | otherwise    = pred n xs

--dire che m n sono consecutivi
tripledifibo :: [(Int, Int, Int)]
tripledifibo = (1,1,2):[(n,m,f) | (_,m',f') <- tripledifibo, (n'',m'', f'') <- tripledifibo, n <- [0..5], m <- [0..5], f <- [0..5], n==f', m==f'', f==m+n, f'==m'', m'==n''] 

