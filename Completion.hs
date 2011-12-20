module Completion where

import Command
import Derivative
import Data.String.Utils (strip)
import Data.Tree
import Data.List (intersperse)
import Control.Monad (join)
import Text.PrettyPrint.HughesPJ

sjoin s = concat . (intersperse s)

data DocsLevel = Word | Line | Page

docs :: DocsLevel -> ArgType -> String
{-docs _ ATInt = "[int]"
docs _ ATString = "[string]"
docs _ ATFile = "[file]"
docs _ (ATToken s) = strip s
docs Word (ATEither args) = "[options]"
docs Line (ATEither args) = join " \t" (map (docs Word) args)
docs Page (ATEither args) = join "\n" (map (docs Line) args)
docs x (ATSet y) = docs x (ATEither y)
docs Word (ATList arg) = (docs Word arg) ++ "*"
docs Line (ATList arg) = (docs Line arg) ++ " *"
docs Page (ATList arg) = (docs Page arg)
docs Word (ATSeq []) = ""
docs Word (ATSeq (arg:args)) = (docs Word arg) ++ "..."
docs Line (ATSeq args) = join " " (map (docs Word) args)
docs Page (ATSeq args) 
  = (docs Line (ATSeq args)) ++ "\n\n" ++ (join "\n" (map (docs Line) args))
docs Word (ATDocumented arg s) = if length s > 20 then docs Word arg else s
docs Line (ATDocumented arg s) = (docs Word arg) ++ " : " ++ s
docs Page (ATDocumented arg s) = s ++ "\n" ++ (docs Page arg)
docs _ ATEmptyStr = ""
docs _ ATFail = ""-}

docTree :: ArgType -> (Tree String)
docTree ATInt                = Node "[int]" []
docTree ATString             = Node "[string]" []
docTree ATFile               = Node "[file]" []
docTree (ATToken s)          = Node s []
docTree (ATEither args)      = Node "" (map docTree args)
docTree (ATSeq args)         = Node "" (map docTree args)
docTree (ATSet args)         = Node "" (map docTree args)
docTree (ATList arg)         = docTree arg
docTree (ATDocumented arg s) = Node s [docTree arg]
docTree (ATEmptyStr)         = Node "" []
docTree (ATFail)             = Node "" []

printTree :: (Tree String) -> String
printTree t = sjoin "\n" (filter ((/="").strip) (aux 12 t)) where
  aux (-1)  _           = [] :: [String]
  aux depth (Node n cs) = n : (map ("  "++) (concatMap (aux (depth-1)) cs))
 

docs _ arg = printTree (docTree arg)

unify :: Eq a => [a] -> Maybe a
unify [] = Nothing
unify (a:as) = if all (a==) as then Just a else Nothing

requiredNextChar :: ArgType -> Maybe Char
requiredNextChar (ATToken (x:xs)) = Just x
requiredNextChar (ATEither args) = join $ unify $ map requiredNextChar args
requiredNextChar (ATSeq (a:as)) = requiredNextChar a
requiredNextChar (ATDocumented arg s) = requiredNextChar arg
requiredNextChar _ = Nothing

requiredNextString :: ArgType -> String
requiredNextString arg = case (requiredNextChar arg) of
  Nothing -> []
  Just c -> c : (requiredNextString $ derivativeWRTChar (makeWS c) arg)