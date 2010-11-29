module ExpParser where

import Parser

expr, term, factor :: Parser Int

expr =
    do t <- term
       do symbol "+"
          e <- expr
          return (t + e)
         +++ return t

term =
    do f <- factor
       do symbol "*"
          t <- term
          return (f * t)
         +++ return f

factor =
    do symbol "("
       e <- expr
       symbol ")"
       return e
      +++ natural

eval :: String -> Int
eval xs =
    case parse expr xs of
        [(n,[])] -> n
        [(_,out)] -> error ("unused input " ++ out)
        [] -> error "invalid input"
