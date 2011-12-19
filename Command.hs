module Command where

data ArgType = ATInt | ATString | ATFile | ATInetAddr | ATRegex
             | ATToken String -- require the token, return ()
             | ATMaybe ArgType
             | ATEither [ArgType]
             | ATSet [ArgType] -- options in any order
             | ATList ArgType
             | ATSeq [ArgType]
             | ATDocumented ArgType String


schema :: ArgType
schema = ATEither [
   ATSeq [ATDocumented (ATToken "echo") "Write arguments to the standard output",
          ATSet [ATDocumented (ATToken "-n") "Do not print trailing newline"],
          ATList ATString],
   ATSeq [ATDocumented (ATToken "cat") "Concatenate and print files",
          ATSet [ATDocumented (ATToken "-b") "Number the non-blank output lines, starting at 1",
                 ATDocumented (ATToken "-e") "Display non-printing characters (like -v) and EOL",
                 ATDocumented (ATToken "-n") "Number the output lines, starting at 1",
                 ATDocumented (ATToken "-s") "Squeeze multiple adjacent empty lines",
                 ATDocumented (ATToken "-t") "Display non-printing characters (like -v) and tabs",
                 ATDocumented (ATToken "-u") "Disable output buffering",
                 ATDocumented (ATToken "-v") "Display non-printing characters"],
          ATList ATFile]]

-- progname args_without_progname
data UntypedCommandData = UntypedCommandData String [String]
data TypedCommandData = TypedCommandData String [ArgType]

data FgBg = Fg | Bg

data Command a = CmdPipe (Command a) (Command a) -- a | b
               | CmdNop
               | CmdRun FgBg a
               | CmdStdinFile String -- place at beginning of pipe
               | CmdStdoutFile String -- place at end of pipe

-- TODO stderr? HSH doesn't support it anyway though

type TypedCommand = Command TypedCommandData
type UntypedCommand = Command UntypedCommandData