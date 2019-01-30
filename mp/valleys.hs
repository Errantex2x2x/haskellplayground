--Solution of https://www.hackerrank.com/challenges/counting-valleys/problem
--Tsoding's solution: https://www.youtube.com/watch?v=Cx0v-r6MsLw

toNumbers :: String -> [Int]
toNumbers xs = map aux xs
    where 
        aux a | a == 'U' = 1
              | a == 'D' = -1

toAbs :: [Int] -> [Int]       --I manually implemented the function scanl which is a variant of foldl
toAbs xs = aux 0 (0 : xs)     --We start from sea level (you can easily use scanl instead of adding a zero...)
    where
        aux s [] = []
        aux s (x:xs) = (s + x) : aux (s + x) xs 

countValleys :: [Int] -> Int
countValleys xs | even $ length $ xs = 0
                | otherwise          = (\x -> x `div` 2) . length $ filter (\x -> x == (0,-1) || x == (-1,0)) $ zip xs $ tail xs

solve :: String -> Int 
solve = countValleys . toAbs . toNumbers

main :: IO ()
main = do
    n <- getLine --Useless, blame HackerRank
    xs <- getLine 
    print $ solve xs
