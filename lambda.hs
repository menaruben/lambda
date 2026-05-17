import Data.Char (isAlphaNum, isSpace)
import Data.List (intersect, nub, (\\))
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

data Expr
  = Var String
  | Fun String Expr
  | App Expr Expr
  deriving (Show, Eq)

data Token
  = LParen
  | RParen
  | Identifier (String)
  | Dot
  | Lambda
  deriving (Show)

prettyShow :: Expr -> String
prettyShow expr = prettyHelper expr 0
  where
    prettyHelper (Var v) indent =
      (pad indent) ++ "var: " ++ v
    prettyHelper (Fun param expr) indent =
      (pad indent) ++ "fun: " ++ param ++ "\n" ++ (prettyHelper expr (indent + 2))
    prettyHelper (App e1 e2) indent =
      (pad indent) ++ "app:\n" ++ (prettyHelper e1 (indent + 2)) ++ "\n" ++ (prettyHelper e2 (indent + 2))

    pad n = replicate n ' '

main :: IO ()
main = do
  args <- getArgs
  source <- case args of
    ["-c", s] -> return s
    _ -> error "repl not implemented yet"

  let ast = parse $ tokenize source
      isValid = validate ast

  putStrLn $
    if isValid
      then (prettyShow ast)
      else "invalid ast" -- TODO: better diagnostics

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | isSpace c = tokenize cs
  | c == '(' = LParen : tokenize cs
  | c == ')' = RParen : tokenize cs
  | c == '.' = Dot : tokenize cs
  | c `elem` ['\\', 'λ'] = Lambda : tokenize cs
  | isAlphaNum c =
      let (word, rest) = span isAlphaNum (c : cs)
       in Identifier word : tokenize rest
  | otherwise = error $ "unexpected char: `" ++ [c] ++ "`"

parse :: [Token] -> Expr
parse tokens = fst $ parseHelper tokens
  where
    parseHelper (t : ts) = case t of
      LParen -> parseApplication ts
      Lambda -> parseFun ts
      Identifier id -> parseVar (t : ts)
      _ -> error $ "unexpected token: " ++ (show t)

    parseApplication tokens =
      let (e1, rest1) = parseHelper tokens
          (e2, rest2) = parseHelper rest1
       in case rest2 of
            (RParen : rest3) -> ((App e1 e2), rest3)
            (r : _) -> error $ "expected closing `)` after app, got: " ++ (show r)

    parseFun (Identifier param : Dot : rest) =
      let (e, rest1) = parseHelper rest
       in ((Fun param e), rest1)
    parseFun t = error $ "unexpected token while parsing fun: " ++ (show t)

    parseVar (Identifier id : rest) = ((Var id), rest)
    parseVar t = error $ "unexpected token while parsing var: " ++ (show t)

distinct [] = []
distinct (x : xs)
  | x `elem` xs = distinct xs
  | otherwise = x : distinct xs

validate :: Expr -> Bool
validate expr = shadowsParameters expr

-- Shadowing parameters should not be allowed (e.g. \x.\y.\x.(y x))
-- TODO: better diagnostics, what exactly is shadowed?
-- returns true if ok, false otherwise
shadowsParameters :: Expr -> Bool
shadowsParameters expr = shadows expr []
  where
    shadows (Fun param expr) acc
      | param `elem` acc = False
      | otherwise = shadows expr (param : acc)
    shadows (Var v) acc = True
    shadows (App e1 e2) acc =
      (shadows e1 acc) && (shadows e2 acc)

freeVars :: Expr -> [String]
freeVars (Var v) = [v]
freeVars (Fun p e) = nub $ freeVars e \\ [p]
freeVars (App e1 e2) = nub $ freeVars e1 ++ freeVars e2

boundVars :: Expr -> [String]
boundVars (Var v) = []
boundVars (Fun p e) = nub $ (p : boundVars e)
boundVars (App e1 e2) = nub $ boundVars e1 ++ boundVars e2

uniqueVar :: String -> [String] -> String
uniqueVar name conflicts
  | name `elem` conflicts = uniqueVar (name ++ "'") conflicts
  | otherwise = name

{-
  since \x.x and \y.y are the "same" function, we can utilize this in
  alpha reductions by renaming the body and parameter of functions
  if the bound variables of e1 conflict with the free variables of the arg e2
  in the application (e1 e2)

  Example:
  (\x.\y.x y)
  e1: \x.\y.x
  e2: y

  - The bound variables in the body of e1 (excluding its own parameter) are {y}
  - The free variables in e2 are {y}
  - So the "conflict"/intersection would be {y}

  if we directly applied the function to y then we would get:
  \y.y

  but now y is bound when it is supposed to be a free variable because
  we replaced each bound x with the free `y`... So instead we have to
  transform the expression to not have any intersection of bound vars in e1
  and free vars in e2:
  (\x.\y'.x y)

  if we apply the function to y now we would get:
  \y'.y

  Now we have a bound variable `y'` and a free variable `y` without conflicts
-}
alphaReduction :: Expr -> Expr
alphaReduction (App (Fun p expr) arg) =
  let argFreeVars = freeVars arg
      funFreeVars = boundVars expr \\ [p]
      conflicting = intersect funFreeVars argFreeVars
   in App (Fun p (subNewNames expr conflicting)) arg
  where
    subNewNames :: Expr -> [String] -> Expr
    subNewNames (Var v) conflicts = Var (uniqueVar v conflicts)
    subNewNames (Fun p e) conflicts = Fun (uniqueVar p conflicts) (subNewNames e conflicts)
    subNewNames (App e1 e2) conflicts = App (subNewNames e1 conflicts) (subNewNames e2 conflicts)
alphaReduction (App e1 e2) = App (alphaReduction e1) (alphaReduction e2)
alphaReduction (Fun p e) = Fun p (alphaReduction e)
alphaReduction expr = expr

{-
  TODO: add some description

  right now betaReduction only does a single step of a betaReduction so we have to reduceUntil..
  prevExpr == currExpr
-}
substitute :: Expr -> String -> Expr -> Expr
substitute (Var v) id expr = if id == v then expr else Var v
substitute (Fun param body) id expr = Fun param (substitute body id expr)
substitute (App e1 e2) id expr = App (substitute e1 id expr) (substitute e2 id expr)

betaReduction :: Expr -> Expr
betaReduction (Var v) = Var v
betaReduction (Fun p e) = Fun p (betaReduction e)
betaReduction (App (Fun p body) e) = substitute body p e
betaReduction (App e1 e2) = App (betaReduction e1) (betaReduction e2)

-- \x.(f x) == f
etaReduction :: Expr -> Expr
etaReduction expr = error "todo"

-- nice to have some builtin things for numerals, bools etc...
deltaReduction :: Expr -> Expr
deltaReduction expr = error "todo"

data ReductionKind = Alpha | Beta | Eta | Delta deriving (Show)

reduceUntil :: (Expr -> Expr -> Bool) -> (Expr -> Expr) -> ReductionKind -> Expr -> [(ReductionKind, Expr)]
reduceUntil predicate reducer kind expr = go predicate reducer kind expr []
  where
    go predicate reducer kind expr acc =
      let result = reducer expr
          shouldContinue = predicate expr result
       in if shouldContinue
            then go predicate reducer kind result ((kind, result) : acc)
            else acc

eval :: Expr -> [(ReductionKind, Expr)]
eval expr =
  let alphaSteps = reduceUntil (\prev curr -> prev /= curr) alphaReduction Alpha expr
      lastAlpha = if null alphaSteps then expr else snd (last alphaSteps)
      betaSteps = reduceUntil (\prev curr -> prev /= curr) betaReduction Beta lastAlpha
   in alphaSteps ++ betaSteps
