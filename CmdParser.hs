module CmdParser where

import Command
import HSH

tokenize :: String -> [String]
tokenize _ = undefined

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy f list = first : splitBy f (dropWhile f rest) where
  (first, rest) = break f list

parseLine :: String -> UntypedCommand
parseLine ""     = CmdNop
parseLine s = CmdRun (UntypedCommandData com args)
  where
    (com:args) = splitBy (== ' ') s
    
                
{-
execCmd :: RunResult b => UntypedCommand -> b
execCmd CmdNop = return ()
execCmd (CmdRun (UntypedCommandData com args)) = run (com, args)
execCmd (CmdSeq (UntypedCommandData c1 a1) 
               (UntypedCommandData c2 a2)) = run (c1, a1) >> run (c2, a2)
execCmd (CmdPipe (UntypedCommandData c1 a1) 
                (UntypedCommandData c2 a2)) = run $ (c1, a1) -|- run (c2, a2)

toShellCommand :: ShellCommand b => Command a -> b
toShellCommand CmdNop = (\_ -> return ())
toShellCommand (CmdRun a) = case a of
  (UntypedCommandData com args) -> (com:args)
  (TypedCommandData com args)   -> undefined
  _                             -> undefined
toShellCommand (CmdSeq com1 com2) = undefined
toShellCommand (CmdPipe com1 com2) = PipeCommand (toShellCommand com1) 
                                                 (toShellCommand com2)
-}

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