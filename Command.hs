module Command where

data ArgType = ATInt | ATString | ATFile | ATInetAddr | ATRegex
             | ATToken String -- require the token, return (). the token should end in " " to require whitespace
             | ATMaybe ArgType
             | ATEither [ArgType]
             | ATSet [ArgType] -- options in any order
             | ATList ArgType
             | ATSeq [ArgType]
             | ATDocumented ArgType String

tokWS :: ArgType
tokWS = ATToken " "

data FgBg = Fg | Bg

-- progname args_without_progname
data Invocation = Invocation String [String]

--                       redirect in    pipeline     redirect out
data Command = Pipeline (Maybe String) [Invocation] (Maybe String) FgBg

-- TODO stderr? HSH doesn't support it anyway though