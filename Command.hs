module Command where

data ArgType = ATInt | ATString | ATFile | ATInetAddr | ATRegex
             | ATToken String -- require the token, return ()
             | ATMaybe ArgType
             | ATEither [ArgType]
             | ATSet [ArgType] -- options in any order
             | ATList ArgType
             | ATSeq [ArgType]

schema :: ArgType
schema = ATEither [
           ATSeq [ATToken "echo", ATSet [ATToken "-n"], ATList ATString],
           ATSeq [ATToken "cat", ATList ATFile]
         ]

-- progname args_without_progname
data UntypedCommandData = UntypedCommandData String [String]
data TypedCommandData = TypedCommandData String [ArgType]

data Command a = CmdPipe (Command a) (Command a) -- a | b
               | CmdSeq (Command a) (Command a)  -- a; b
               | CmdNop
               | CmdRun a

type TypedCommand = Command TypedCommandData
type UntypedCommand = Command UntypedCommandData