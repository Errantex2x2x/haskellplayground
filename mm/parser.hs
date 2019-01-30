import Data.Char (isDigit, isSpace)

type Parser a = String -> [(a, String)]

just :: a -> Parser a
just v cs = [(v, cs)]

only :: (Char -> Bool) -> Parser Char
only f (c : cs) | f c = [(c, cs)]
only _ _ = []

digit :: Parser Char
digit = only isDigit

space :: Parser Char
space = only isSpace

char :: Char -> Parser Char
char c = only (== c)

(>>>) :: Parser a -> (a -> Parser b) -> Parser b
(>>>) p f = concat . map (uncurry f) . p

(&&&) :: Parser a -> Parser b -> Parser b
(&&&) p q = p >>> const q

(|||) :: Parser a -> Parser a -> Parser a
(|||) p q cs = p cs ++ q cs

list :: Parser a -> Parser [a]
list p = just [] ||| list1 p

list1 :: Parser a -> Parser [a]
list1 p = p >>> \v -> list p >>> \vs -> just (v : vs)

spaces :: Parser ()
spaces = list space &&& just ()

digits :: Parser String
digits = list1 digit

token :: Parser a -> Parser a
token p = spaces &&& p

symbol :: Char -> Parser Char
symbol c = token (char c)

int :: Parser Int
int = token digits >>> \cs -> just (read cs)

parens :: Parser a -> Parser a
parens p = symbol '(' &&& p >>> \v -> symbol ')' &&& just v

expr :: Parser Int
expr = term ||| (term >>> \m -> symbol '+' &&& expr >>> \n -> just (m + n))
 
term :: Parser Int
term = fact ||| (fact >>> \m -> symbol '*' &&& term >>> \n -> just (m * n))

fact :: Parser Int
fact = int ||| parens expr