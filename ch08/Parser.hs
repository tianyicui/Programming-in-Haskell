module Parser where

import Prelude (Char, String, Monad(..))

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
