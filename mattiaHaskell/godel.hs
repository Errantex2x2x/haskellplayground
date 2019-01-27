type Alphabet = String

default_alphabet = ['a'..'z']++['A'..'Z']++['0'..'9']

godelify :: Alphabet -> String -> [Int]
godelify a ws = map aux (godelifyS a ws)
  where
    aux :: [(Int,Int)] -> Int
    aux [] = 1
    aux (x:xs) = case x of (a,b) -> (a^b) * (aux xs)
    
godelifyS :: Alphabet -> String -> [[(Int, Int)]]
godelifyS a ws = map (godelifyW a) (words ws)

godelifyW :: Alphabet -> String -> [(Int,Int)]
godelifyW alphabet =  aux 1
  where
    aux :: Int -> String -> [(Int,Int)]
    aux _ [] = []
    aux n (x:xs) = ((takePrime n),(getIndex alphabet x)) : (aux (n+1) xs)

    getIndex :: (Eq a) => [a] -> a -> Int
    getIndex (x:xs) y | y==x      = 1
                      | otherwise = 1 + getIndex xs y
-- Crivello di Eratostene
takePrime :: Int -> Int
takePrime = takePrimeAux [2..]
  where
    takePrimeAux :: [Int] -> Int -> Int
    takePrimeAux (l:ls) 1 = l
    takePrimeAux (l:ls) n = takePrimeAux (filter (\x -> (mod x l) /= 0) ls) (n-1)

