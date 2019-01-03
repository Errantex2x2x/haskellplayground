hangman :: IO ()
hangman = do {
    secret <- getSecretWord;
    chances <- getChances;
    play secret [' '] chances
}

play :: String -> [Char] -> Int -> IO ()
play secret letters chances = do {
    printSpace 200;
    if chances <= 0 then
        putStrLn ("\n\n --- You lost. The word was " ++ secret)
    else do {
        putStrLn ("You have " ++ (show chances) ++ " tries left. Played letters: " ++ (formattedLetters letters));
        putStrLn (match secret letters);
        if (match secret letters) == secret then
            putStrLn "\n\n **** YOU WON! ****"
        else do {
            putStr "\n\nPick a letter: ";
            c <- readChar;
            play secret (c:letters) (if elem c secret then chances else (chances-1))
            }
        }
    }

getSecretWord :: IO String
getSecretWord = do {
   putStrLn "Pick a secret word or phrase:";
   xs <- readword;
   return xs
    }

getChances :: IO Int
getChances = do {
    putStrLn "How many chances does the player have?";
    s <- getLine;
    return (read s :: Int)
    }

printSpace:: Int -> IO ()
printSpace n = do {
    if n > 0 then
        do { 
            putStrLn "";
            printSpace (n - 1)
        }
    else
        putStrLn ""
}

readChar :: IO Char
readChar = do {
    s <- getLine;
    return (head (s ++ " "))
    }

readword :: IO String
readword = do {
        xs <- getLine;
        return xs 
        }

formattedLetters:: [Char] -> String
formattedLetters [x] = [x]
formattedLetters (x:xs) = x : ',' : formattedLetters xs
formattedLetters _ = ""



match :: String -> [Char] -> String
match secret letters = [if elem x letters || x == ' ' then x else '_' | x <- secret]