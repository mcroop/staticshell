{-# LANGUAGE FlexibleInstances #-}

module CmdParser where

import HSH
import Command

import Data.Maybe (fromJust)
import Data.Char
import Data.List (intersperse)
import Control.Concurrent

{- instance Show Invocation where
  show (Invocation cmd args) = concat $ intersperse " " (cmd:args)
  
instance Show Command where
  show (Pipeline redirIn inv redirOut fgbg)
    = concat $ intersperse " " ["Pipeline", 
                                (show redirIn),
                                (show inv),
                                (show redirOut),
                                (show fgbg)]
-}

{- invoke a single command (invocation) -}
instance ShellCommand Invocation where
  fdInvoke (Invocation cmd args) = fdInvoke (cmd, args)

{- make us able to pipe multiple operations together as ShellCommands -}
instance ShellCommand [Invocation] where
  fdInvoke []         env ichan = fdInvoke ("echo", ([]::[String])) env ichan
  fdInvoke [inv]      env ichan = fdInvoke inv env ichan
  fdInvoke (inv:invs) env ichan = fdInvoke (PipeCommand inv invs) env ichan

{- build a command out of a String -}
buildCmd :: String -> Command
buildCmd s = pipeSplit
  where
    toks' = doTokenize s
    lTok | toks' == [] = ""
         | otherwise   = last toks'
    toks | lTok == "&" = init toks'
         | otherwise = toks'
    fb | lTok == "&" = Bg
       | otherwise   = Fg
    pipeSplit = foldl aux (Pipeline Nothing [] Nothing fb) toks
    aux (Pipeline i invs o fgbg) x 
      | x == "|"  = Pipeline i (invs ++ [(Invocation "" [])]) o fgbg
      | otherwise = case invs of
        [] -> Pipeline i [(Invocation x [])] o fgbg
        _  -> case (last invs) of
          (Invocation "" as) -> Pipeline i ((init invs) ++ [(Invocation x as)]) o fgbg
          (Invocation c  as) -> Pipeline i ((init invs) ++ [(Invocation c (as ++ [x]))]) o fgbg

{- 
an accumulator-style tokenizing helper function

meant to be called through doTokenize 
-}
tokenize' :: Maybe Char -> -- previous character
            Maybe Char -> -- current character
            Bool ->       -- quoted
            String ->     -- current token
            [String] ->   -- accumulated tokens
            String ->     -- remaining input
            [String]      -- result
tokenize' pr Nothing True tok toks []      = undefined
tokenize' pr Nothing _    tok toks []      = if tok == "" then toks else toks ++ [tok]
tokenize' pr cr      qu   tok toks (r:rem) = tokenize' pr' cr' qu' tok' toks' rem'
  where
    pr'   = if pr == (Just '\\') && cr == (Just '\\') 
              then (Just 'a') 
            else cr
    cr'   = if rem == [] 
              then Nothing 
            else 
              Just r
    qu'   = if cr == (Just '"') && not (pr == (Just '\\'))
              then not qu
            else qu
    tok'  = if (cr == (Just '"') && not (pr == (Just '\\'))) || 
               (cr == (Just '\\') && not (pr == (Just '\\')))
              then tok 
            else if not qu && cr == (Just ' ') 
                   then "" 
                 else tok ++ [fromJust cr]
    toks' = if (cr == (Just ' ') && not qu ) || rem == [] 
              then if tok == "" then toks else toks ++ [tok] 
            else
              toks
    rem'  = rem

{- 
tokenize a string, triming whitespace.

doTokenize "hello world" == ["hello", "world"]

whitespace within quotation marks does not split tokens

doTokenize "this \"is a quote\"" == ["this", "is a quote"]

quoted string without a preceding/following space are included with the
preceding/following token

doTokenize "hello wo\"a whole new\"rld" == ["hello", "woa whole newrld"]

-}
doTokenize :: String -> [String]
doTokenize []  = []
doTokenize [c] = [[c]]
doTokenize str = tokenize' Nothing (Just s) False "" [] ss 
  where
    (s:ss) = str ++ "  " -- the kludgey way to fix tokenizer

