module Main (main) where

import Preprocessor (preprocess)
import Parser (parse, prettyAstView, Expr(..), betaReduction)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)
import Tokenizer (tokenize)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  source <- case args of
    ["-c", s] -> return s
    ["-f", filePath] -> readFile filePath
    -- TODO: enter REPL here instead
    _ -> error "repl not implemented yet"

  putStrLn $ (prettyAstView . fst . betaReduction . fst . parse . tokenize . preprocess) source
