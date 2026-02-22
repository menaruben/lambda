module Parser (Expr (..), parse, prettyAstView, betaReduction) where

import Tokenizer

data Expr
  = IdExpr String
  | FuncExpr String Expr
  | ApplExpr Expr Expr
  deriving (Show)

prettyAstView :: Expr -> String
prettyAstView expr = prettyHelper 0 expr
  where
    prettyHelper indent (IdExpr x) = pad indent ++ "Id: " ++ x
    prettyHelper indent (FuncExpr x body) =
      pad indent ++ "Func: " ++ x ++ "\n" ++ prettyHelper (indent + 2) body
    prettyHelper indent (ApplExpr e1 e2) =
      pad indent ++ "App:\n" ++ prettyHelper (indent + 2) e1 ++ "\n" ++ prettyHelper (indent + 2) e2

    pad n = replicate n ' '

parse :: [Token] -> (Expr, [Token])
parse (LParen : rest) = parseApplication rest
parse (Lambda : rest) = parseFunc rest
parse (Identifier id : rest) = (IdExpr id, rest)
parse (RParen : _) = error "unexpected `)`"
parse (Dot : _) = error "unexpected `.`"
parse [] = error "unexpected end of input"

parseApplication :: [Token] -> (Expr, [Token])
parseApplication tokens =
  let (e1, rest1) = parse tokens
      (e2, rest2) = parse rest1
   in case rest2 of
        (RParen : rest3) -> (ApplExpr e1 e2, rest3)
        _ -> error "expected closing `)` after application"

parseFunc :: [Token] -> (Expr, [Token])
parseFunc (Identifier param : Dot : rest) =
  let (body, remaining) = parse rest
   in (FuncExpr param body, remaining)
parseFunc tokens = error ("expected param.body after λ, got: " ++ show tokens)

-- returns a tuple where
-- fst: final expression
-- snd: reduction history
betaReduction :: Expr -> (Expr, [Expr])
betaReduction expr = bReduce expr []
  where
    bReduce (ApplExpr (FuncExpr param body) arg) history =
      let result = substitute param arg body
       in bReduce result (history ++ [result])
      
    bReduce (ApplExpr e1 e2) history =
      let (e1', h1) = bReduce e1 history
       in case e1' of
            FuncExpr param body -> bReduce (substitute param e2 body) h1
            _ ->
              let (e2', h2) = bReduce e2 h1
               in (ApplExpr e1' e2', h2)

    bReduce (FuncExpr param body) history =
      let (body', h) = bReduce body history
       in (FuncExpr param body', h)

    bReduce expr history = (expr, history)

substitute :: String -> Expr -> Expr -> Expr
substitute name arg (ApplExpr e1 e2) =
  ApplExpr (substitute name arg e1) (substitute name arg e2)

substitute name arg (FuncExpr param body)
  | param == name = FuncExpr param body -- shadowed
  | otherwise = FuncExpr param (substitute name arg body)

substitute name arg (IdExpr x)
  | x == name = arg
  | otherwise = IdExpr x
