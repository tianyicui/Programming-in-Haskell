module Parser where

import Prelude (Char, String)

type Parser a = String -> [(a, String)]

return :: a -> Parser a
return v = \inp -> [(v,inp)]

failure :: Parser a
failure = \inp -> []

item :: Parser Char
item = \inp -> case inp of
                    [] -> []
                    (x : xs) -> [(x, xs)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = \inp -> case parse p inp of
                       [] -> []
                       [(v,out)] -> parse (f v) out

p0 :: Parser (Char, Char)
p0 =
    item >>= \x ->
    item >>= \_ ->
    item >>= \y ->
    return (x, y)

-- is equivalent to
-- p1 :: Parser (Char, Char)
p1 = do
    x <- item
    item
    y <- item
    return (x, y)
