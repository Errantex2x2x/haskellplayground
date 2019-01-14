--Solution of: https://www.hackerrank.com/challenges/electronics-shop/problem

solve :: [Int] -> [Int] -> Int -> Int
solve xs ys b = case listsums of
                    [] -> -1
                    _ -> maximum listsums
    where 
        listsums = filter (<= b) (aux xs ys)
        aux xs (y:ys) = (map (+ y) xs) ++ (aux xs ys) 
        aux _ _       = []

solveListComprehension :: [Int] -> [Int] -> Int -> Int
solveListComprehension xs ys b = case listsums of
                                    [] -> -1
                                    _ -> maximum $ listsums
    where 
        listsums = filter (<= b) $ map (uncurry (+)) [ (a, b) | a <- xs, b <- ys]