module Tokenizer (tokenize, Token (..)) where

import Data.Char (isAlphaNum)

data Token
  = LParen
  | RParen
  | Identifier (String)
  | Dot
  | Lambda
  deriving (Show)

data TokenizerCtx = TokenizerCtx
  { source :: String,
    index :: Int,
    tokens :: [Token]
  }
  deriving (Show)

-- TODO:
-- use unicode to support actual lambdaa
-- preserve position  data in token for better errors
getNextToken :: TokenizerCtx -> TokenizerCtx
getNextToken (TokenizerCtx {source = s, index = i, tokens = ts}) = case s !! i of
  ' ' -> getNextToken (TokenizerCtx {source = s, index = i + 1, tokens = ts})
  '(' -> TokenizerCtx {source = s, index = i + 1, tokens = ts ++ [LParen]}
  ')' -> TokenizerCtx {source = s, index = i + 1, tokens = ts ++ [RParen]}
  '.' -> TokenizerCtx {source = s, index = i + 1, tokens = ts ++ [Dot]}
  '\\' -> TokenizerCtx {source = s, index = i + 1, tokens = ts ++ [Lambda]}
  'λ' -> TokenizerCtx {source = s, index = i + 1, tokens = ts ++ [Lambda]}
  c ->
    if (isAlphaNum c)
      then getNextId (TokenizerCtx {source = s, index = i, tokens = ts})
      else error ("unexpected char: `" ++ [c, '`'])

getNextId :: TokenizerCtx -> TokenizerCtx
getNextId (TokenizerCtx {source = s, index = i, tokens = ts}) =
  TokenizerCtx
    { source = s,
      index = i + length id,
      tokens = ts ++ [Identifier id]
    }
  where
    id = takeWhile isAlphaNum (drop i s)

tokenize :: String -> [Token]
tokenize s = tokens $ until done getNextToken initial
  where
    initial = TokenizerCtx {source = s, index = 0, tokens = []}
    done ctx = index ctx >= length s
