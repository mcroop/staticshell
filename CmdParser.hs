module CmdParser where

import HSH
import Command
import Data.Maybe
import Data.Char

--tokenize :: String -> [String]
--tokenize _ = undefined

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list

{- TODO update to a real parser -}
--parseLine :: String -> UntypedCommand
--parseLine ""     = CmdNop
--parseLine s = CmdRun (UntypedCommandData com args)
--  where
--    (com:args) = splitBy (== ' ') s
    
{- need to change since Command has a new structure -}
--execCmd :: RunResult b => Command -> b
execCmd :: Command -> IO ()
-- execCmd (PipeLine redirIn (Invocation cmd args) redirOut fgbg)
execCmd (Pipeline redirIn [Invocation cmd args] redirOut _)
  = runIO (cmd, args)

{-
execCmd CmdNop = return ()
execCmd (CmdRun (UntypedCommandData com args)) = run (com, args)
execCmd (CmdSeq (UntypedCommandData c1 a1) 
               (UntypedCommandData c2 a2)) = run (c1, a1) >> run (c2, a2)
execCmd (CmdPipe (UntypedCommandData c1 a1) 
                (UntypedCommandData c2 a2)) = run $ (c1, a1) -|- run (c2, a2)
-}

{-
class (Show a) => ShellCommand a where
    {- | Invoke a command. -}
    fdInvoke :: a               -- ^ The command
             -> Environment     -- ^ The environment
             -> Channel         -- ^ Where to read input from
             -> IO (Channel, [InvokeResult]) -- ^ Returns an action that, when evaluated, waits for the process to finish and returns an exit code.
-}

--toShellCommand :: ShellCommand b => Command -> b
--toShellCommand (Pipeline Nothing (Invocation "" _) Nothing _) = return ()
-- toShellCommand (Pipeline Nothing (Invocation cmd args) 

{-
type TypedCommand = Command TypedCommandData
type UntypedCommand = Command UntypedCommandData
-}
                                   
{-
instance ShellCommand (Command TypedCommandData) where
  fdInvoke _ _ _ = return ()

instance ShellCommand (Command UntypedCommandData) where
  fdInvoke _ _ _ = return ()
-}

tokenize :: Maybe Char -> -- previous character
            Maybe Char -> -- current character
            Bool ->       -- quoted
            String ->     -- current token
            [String] ->   -- accumulated tokens
            String ->     -- remaining input
            [String]      -- result
{-tokenize _           Nothing     True  _   _    _       = undefined
tokenize _           Nothing     _     tok toks _       = toks ++ [tok]
tokenize (Just '\\') (Just '"')  qu    tok toks (r:rem) = tokenize (Just '"') (Just r) qu    (tok++['"'])  toks          rem
tokenize _           (Just '"')  True  tok toks (r:rem) = tokenize (Just '"') (Just r) False tok           toks          rem
tokenize _           (Just '"')  _     tok toks (r:rem) = tokenize (Just '"') (Just r) True  tok           toks          rem
tokenize _           (Just ' ')  True  tok toks (r:rem) = tokenize (Just ' ') (Just r) True  (tok++[' '])  toks          rem
tokenize _           (Just ' ')  _     tok toks (r:rem) = tokenize (Just ' ') (Just r) False ""            (toks++[tok]) rem
tokenize (Just '\\') (Just '\\') qu    tok toks (r:rem) = tokenize (Just 'a') (Just r) qu    (tok++['\\']) toks          rem
tokenize pr          (Just cr)   qu    tok toks []      = tokenize (Just cr)  Nothing  qu    (tok++[cr])   toks          []
tokenize _           (Just cr)   qu    tok toks (r:rem) = tokenize (Just cr)  (Just r) qu    (tok++[cr])   toks          rem-}
tokenize pr Nothing True tok toks []      = undefined
tokenize pr Nothing _    tok toks []      = if tok == "" then toks else toks ++ [tok]
tokenize pr cr      qu   tok toks (r:rem) = tokenize pr' cr' qu' tok' toks' rem'
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
    tok'  = if cr == (Just '"') && not (pr == (Just '\\'))
              then tok 
            else if not qu && cr == (Just ' ') 
                   then "" 
                 else tok ++ [fromJust cr]
    toks' = if (cr == (Just ' ') && not qu ) || rem == [] 
              then if tok == "" then toks else toks ++ [tok] 
            else
              toks
    rem'  = rem


doTokenize :: String -> [String]
doTokenize []  = []
doTokenize [c] = [[c]]
doTokenize str = tokenize Nothing (Just s) False "" [] ss 
  where
    (s:ss) = str

trim      :: String -> String
trim      = f . f
  where 
    f = reverse . dropWhile isSpace