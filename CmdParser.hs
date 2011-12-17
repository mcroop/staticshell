module CmdParser where

import Command

parseLine :: String -> UntypedCommand
parseLine _ = CmdNop