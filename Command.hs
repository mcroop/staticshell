module Command where

data ArgType = ATInt | ATString | ATFile
             | ATToken String -- require the token, return (). the token 
                              -- should end in " " to require whitespace
             | ATEither [ArgType]
             | ATSet [ArgType] -- options in any order
             | ATList ArgType
             | ATSeq [ArgType]
             | ATDocumented ArgType String
             | ATEmptyStr
             | ATFail
  deriving (Show, Eq)

tokWS :: ArgType
tokWS = ATToken " "

atMaybe :: ArgType -> ArgType
atMaybe t = ATEither [ATEmptyStr, t]

data FgBg = Fg | Bg
  deriving Show

-- progname args_without_progname
data Invocation = Invocation String [String]
  deriving Show

--                       redirect in    pipeline     redirect out
data Command = Pipeline (Maybe String) [Invocation] (Maybe String) FgBg
  deriving Show

-- TODO stderr? HSH doesn't support it anyway though