module Parser where

import Char
import Monad

newtype Parser a = P (String -> [(a, String)])

instance Monad Parser where
    return v = P (\inp -> [(v,inp)])
    p >>= f = P (\inp -> case parse p inp of
                            [] -> []
                            [(v,out)] -> parse (f v) out)

failure :: Parser a
failure = P (\inp -> [])

item :: Parser Char
item = P (\inp -> case inp of
                     [] -> []
                     (x : xs) -> [(x, xs)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

p0 :: Parser (Char, Char)
p0 =
    item >>= \x ->
    item >>= \_ ->
    item >>= \y ->
    return (x, y)
-- is equivalent to
p1 :: Parser (Char, Char)
p1 = do
    x <- item
    item
    y <- item
    return (x, y)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = P (\inp -> case parse p inp of
                        [] -> parse q inp
                        [(v,out)] -> [(v,out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else failure

digit, lower, upper, letter, alphanum :: Parser Char
digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (==x)

string :: String -> Parser String
string [] = return []
string (x : xs) = do
    char x
    string xs
    return (x : xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
many1 :: Parser a -> Parser [a]
many1 p = do
    v <- p
    vs <- many p
    return (v : vs)

ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x : xs)

nat :: Parser Int
nat = do
    xs <- many1 digit
    return (read xs)

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

symbol :: String -> Parser String
symbol xs = token (string xs)

-- soooo powerful!
intListParser :: Parser [Int]
intListParser = do
    symbol "["
    n <- natural
    ns <- many (do symbol ","
                   natural)
    symbol "]"
    return (n:ns)
