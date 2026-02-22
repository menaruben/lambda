module Preprocessor (preprocess) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf, stripPrefix)
import Data.Char (isAlphaNum)

preprocess :: String -> String
preprocess source =
  let defines = getDefines source
      cleaned = removeDefines source
  in expandMacros defines cleaned

getDefines :: String -> Map String String
getDefines source = foldr parseLine Map.empty (lines source)
  where
    parseLine line acc
      | ":define " `isPrefixOf` line =
          let rest = drop (length ":define ") line
              name = takeWhile isAlphaNum rest
              body = drop (length name + 1) rest
          in Map.insert name body acc
      | otherwise = acc

removeDefines :: String -> String
removeDefines source = unlines $ filter (not . isPrefixOf ":define ") (lines source)

expandMacros :: Map String String -> String -> String
expandMacros defines [] = []
expandMacros defines ('$' : rest) =
  let name = takeWhile isAlphaNum rest
      remaining = drop (length name) rest
  in case Map.lookup name defines of
    Just body -> body ++ expandMacros defines remaining
    Nothing   -> error ("undefined macro: " ++ name)
expandMacros defines (c : rest) = c : expandMacros defines rest
