import Data.List (sort)

putString :: String -> IO ()
putString = foldr (\x y -> putChar x >> y) (return ())

putString' :: String -> IO ()
putString' = foldr ((>>).putChar) (return ())

putString'' :: String -> IO ()
putString'' xs = foldr (>>) (return ()) (map putChar xs)

readAll :: IO [String]
readAll = do l <- getLine
             if null l
               then return []
               else do ls <- readAll
                       return (l : ls)

sortLines :: IO ()
sortLines = readAll >>= (\x -> print (sort x))

getInt :: IO Int
getInt = getLine >>= (return.read)

getInts :: IO [Int]
getInts = do (n,l) <- (getLine >>= (\l -> return (read l, l)))
             if null l
               then return []
               else do ns <- getInts
                       return (n : ns)
  where
    isZero 0 = True
    isZero _ = False

sortInts :: IO ()
sortInts = getInts >>= (print.sort)
  
somma :: IO ()
somma = getInts >>= (print.sum)
