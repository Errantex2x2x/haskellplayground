--Solution of https://www.hackerrank.com/challenges/drawing-book/problem
--Tsoding's solution: https://www.youtube.com/watch?v=Cx0v-r6MsLw

makeBook :: Int -> [(Int, Int)]
makeBook n = evens $ zip [0..n] $ [1..n] ++ [0]

evens :: [a] -> [a] --mutual recursion
evens (x:xs) = x:odds xs
evens _ = []

odds :: [a] -> [a]
odds (_:xs) = evens xs
odds _ = []

solve :: Int -> Int -> Int
solve n p = min (p `div` 2) $ bookLength - p `div` 2
    where
        bookLength = (\x -> x-1) . length $ makeBook n -- minus one because it's zero based

main :: IO ()
main = do
    n <- getLine
    p <- getLine 
    let x = read n :: Int
    let y = read p :: Int
    print $ solve x y
