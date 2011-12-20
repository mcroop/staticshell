module Derivative where

import Command
import Data.String.Utils

bool2arg :: Bool -> ArgType
bool2arg True = ATEmptyStr
bool2arg False = ATFail

nullable :: ArgType -> Bool
nullable ATEmptyStr = True
nullable (ATEither args) = any nullable args
nullable (ATSeq args) = all nullable args
nullable (ATSet args) = True
nullable (ATList arg) = True
nullable (ATDocumented arg _) = nullable arg
nullable _ = False

cons :: ArgType -> ArgType -> ArgType
cons x y = cons' (remdoc x) (remdoc y) where
  cons' ATEmptyStr x          = x
  cons' x          ATEmptyStr = x
  cons' ATFail     x          = ATFail
  cons' x          ATFail     = ATFail
  cons' (ATSeq a)  (ATSeq b)  = ATSeq (a ++ b)
  cons' (ATSeq a)  b          = ATSeq (a ++ [b])
  cons' a          (ATSeq b)  = ATSeq (a : b)
  cons' a          b          = ATSeq [a, b]

data CharWS = Char Char | WS deriving Eq

makeWS :: Char -> CharWS
makeWS ' '  = WS
makeWS '\t' = WS
makeWS c    = Char c

stringLikeDerivative :: (Char -> Bool) -> ArgType -> ArgType
stringLikeDerivative f defaultArg = if (f '<') || (f '>') || (f '&') || (f '|') then ATFail
                                    else if (not (f ' ')) then atMaybe defaultArg
                                    else ATEmptyStr

derivative :: (Char -> Bool) -> ArgType -> ArgType
derivative f ATInt            = if (any f "0123456789") then atMaybe ATInt else ATFail
derivative f ATString         = stringLikeDerivative f ATString
derivative f ATFile           = stringLikeDerivative f ATFile
derivative f (ATToken "")     = ATFail
derivative f (ATToken (c:"")) = bool2arg (f c)
derivative f (ATToken (c:cs)) = if (f c) then ATToken cs else ATFail
derivative f (ATEither args)  = atsum (map (derivative f) args)
derivative f (ATSet args)     = derivative f (ATList (ATEither args)) --TODO
derivative f (ATList arg)     = cons (derivative f arg) (ATList arg)
derivative f (ATSeq [])       = ATFail
derivative f (ATSeq (t:[]))   = derivative f t
derivative f (ATSeq (t:ts))   = atsum [cons (derivative f t) (ATSeq ts),
                                       cons (bool2arg $ nullable t) 
                                            (derivative f (ATSeq ts))]
derivative f (ATDocumented ATEmptyStr _) = ATFail
derivative f (ATDocumented ATFail _)     = ATFail
derivative f (ATDocumented arg help)     = ATDocumented (derivative f arg) help
derivative _ ATEmptyStr                  = ATFail
derivative _ ATFail                      = ATFail

derivativeWRTChar :: CharWS -> ArgType -> ArgType
derivativeWRTChar a = derivative (\c -> a == (makeWS c))

derivativeNonWS :: ArgType -> ArgType
derivativeNonWS = derivative (\c -> WS /= (makeWS c))

derivatives :: ArgType -> [String] -> ArgType
derivatives arg s = foldl (flip derivativeWRTChar) arg $ 
                      join [WS] (map (map Char) s)

upToWS :: ArgType -> (ArgType, Bool)

upToWS ATInt = (ATInt, False)
upToWS ATString = (ATString, True)
upToWS ATFile = (ATFile, True)
upToWS (ATToken t) = if (any (==' ') t) then
  (ATToken (head $ (splitWs t) ++ [""]), True) else (ATToken t, False)
upToWS (ATEither args) = (atsum (map fst res), all id (map snd res)) where
  res = map upToWS args
upToWS (ATSeq []) = (ATEmptyStr, False)
upToWS (ATSeq (a:as)) = case (upToWS a) of
  (a', True) -> (a', True)
  (a', False) -> (cons a' rest, rest_res) where
    (rest, rest_res) = upToWS (ATSeq as)
upToWS (ATList a) = case (upToWS a) of
  (a', True) -> (a', False)
  (a', False) -> (ATList a, False)
upToWS (ATSet a) = upToWS (ATList (ATEither a))
upToWS (ATDocumented ATEmptyStr _) = upToWS ATEmptyStr
upToWS (ATDocumented ATFail _) = upToWS ATFail
upToWS (ATDocumented a s) = (ATDocumented (fst res) s, snd res) where
  res = upToWS a
upToWS ATEmptyStr = (ATEmptyStr, False)
upToWS ATFail = (ATFail, True)
