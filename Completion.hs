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

elideEmptyHeaders :: Tree String -> Tree String
elideEmptyHeaders (Node "" [c]) = elideEmptyHeaders c
elideEmptyHeaders (Node s cs) = Node s $ concatMap aux cs where
  aux (Node "" cs') = concatMap aux cs'
  aux n = [elideEmptyHeaders n]

printTree :: (Tree String) -> String
printTree t = sjoin "\n" (filter ((/="").strip) (aux 12 t)) where
  aux (-1)  _           = [] :: [String]
  aux depth (Node n cs) = n : (map ("  "++) (concatMap (aux (depth-1)) cs))
 

docs _ = printTree . elideEmptyHeaders . docTree

unify :: Eq a => [a] -> Maybe a
unify [] = Nothing
unify (a:as) = if all (a==) as then Just a else Nothing

data AutocompletionChar = DefiniteChar Char  -- definitely need this character
                        | StopCompletion     -- don't know, should stop
                        | ContinueCompletion -- don't care
                        | FilenameCompletion
  deriving (Eq, Show)
  

isContinueCompl :: AutocompletionChar -> Bool
isContinueCompl ContinueCompletion = True
isContinueCompl FilenameCompletion = True
isContinueCompl _ = False

-- What is the next character that must be typed by the user?
requiredNextChar :: ArgType -> AutocompletionChar
requiredNextChar (ATToken (x:xs)) = DefiniteChar x
requiredNextChar (ATEither args) = case filter (\x -> not $ isContinueCompl x) $ nexts of
  [] -> if any (==FilenameCompletion) nexts then FilenameCompletion else ContinueCompletion
  (a:as) -> if all (a==) as then a else StopCompletion
  where nexts = map requiredNextChar args
requiredNextChar (ATSeq (a:as)) = requiredNextChar a
requiredNextChar (ATDocumented arg s) = requiredNextChar arg
requiredNextChar ATFile = FilenameCompletion
requiredNextChar _ = ContinueCompletion

-- What is the next string that must be typed by the user?
requiredNextString :: ArgType -> String
requiredNextString arg = case (requiredNextChar arg) of
  DefiniteChar c -> c : (requiredNextString $ derivativeWRTChar (makeWS c) arg)
  _ -> []

requiredFilenameCompletion :: ArgType -> Bool
requiredFilenameCompletion arg = case (requiredNextChar arg) of
  FilenameCompletion -> True
  _ -> False