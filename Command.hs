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

data FgBg = Fg | Bg

-- progname args_without_progname
data Invocation = Invocation String [String]

--                       redirect in    pipeline     redirect out
data Command = Pipeline (Maybe String) [Invocation] (Maybe String) FgBg

-- TODO stderr? HSH doesn't support it anyway though