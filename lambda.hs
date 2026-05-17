import Data.Char (isAlphaNum, isSpace)
import Data.List (intersect, nub, (\\))
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)

data Expr
  = Var String
  | Fun String Expr
  | App Expr Expr
  deriving (Show)

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

-- todo
betaReduction :: Expr -> Expr
betaReduction expr = error "todo"

-- nice to have
etaReduction :: Expr -> Expr
etaReduction expr = error "todo"

-- nice to have
deltaReduction :: Expr -> Expr
deltaReduction expr = error "todo"

eval :: Expr -> Expr
eval expr = deltaReduction $ etaReduction $ betaReduction $ alphaReduction expr
