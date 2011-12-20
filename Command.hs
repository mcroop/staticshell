module Command where

import Data.List

-- The type of an argument; the pattern used in the parser.
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

remdoc :: ArgType -> ArgType
remdoc (ATDocumented ATFail _) = ATFail
remdoc (ATDocumented ATEmptyStr _) = ATEmptyStr
remdoc x = x

atMaybe :: ArgType -> ArgType
atMaybe = atplus ATEmptyStr

-- Form an ATEither from two types.
atplus :: ArgType -> ArgType -> ArgType
atplus x y = atplus' (remdoc x) (remdoc y) where
  atplus' ATFail       x            = x
  atplus' x            ATFail       = x
  atplus' (ATEither a) (ATEither b) = ATEither $ nub (a ++ b)
  atplus' (ATEither a) b            = ATEither $ nub (a ++ [b])
  atplus' a            (ATEither b) = ATEither $ nub (a : b)
  atplus' a            b            = ATEither $ nub [a, b]

-- Form an ATEither from several types
atsum :: [ArgType] -> ArgType
atsum = foldl atplus ATFail

data FgBg = Fg | Bg
  deriving Show

-- progname args_without_progname
data Invocation = Invocation String [String]
  deriving Show

--                       redirect in    pipeline     redirect out
data Command = Pipeline (Maybe String) [Invocation] (Maybe String) FgBg
  deriving Show

-- TODO stderr? HSH doesn't support it anyway though