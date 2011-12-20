module Completion where

import Command
import Data.String.Utils
import Data.Tree
import Text.PrettyPrint.HughesPJ

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
docs Page (ATSeq args) = (docs Line (ATSeq args)) ++ "\n\n" ++ (join "\n" (map (docs Line) args))
docs Word (ATDocumented arg s) = if length s > 20 then docs Word arg else s
docs Line (ATDocumented arg s) = (docs Word arg) ++ " : " ++ s
docs Page (ATDocumented arg s) = s ++ "\n" ++ (docs Page arg)
docs _ ATEmptyStr = ""
docs _ ATFail = ""-}

docTree :: ArgType -> (Tree String)
docTree ATInt = Node "[int]" []
docTree ATString = Node "[string]" []
docTree ATFile = Node "[file]" []
docTree (ATToken s) = Node s []
docTree (ATEither args) = Node "" (map docTree args)
docTree (ATSeq args) = Node "" (map docTree args)
docTree (ATSet args) = Node "" (map docTree args)
docTree (ATList arg) = docTree arg
docTree (ATDocumented arg s) = Node s [docTree arg]
docTree (ATEmptyStr) = Node "" []
docTree (ATFail) = Node "" []

printTree :: (Tree String) -> String
printTree t = join "\n" (aux 6 t) where
  aux (-1) _ = [] :: [String]
  aux depth (Node n cs) = n : (map ("  "++) (concatMap (aux (depth-1)) cs))
 

docs _ arg = printTree (docTree arg)