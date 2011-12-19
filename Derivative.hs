module Derivative where

import Command

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

remdoc :: ArgType -> ArgType
remdoc (ATDocumented ATFail _) = ATFail
remdoc (ATDocumented ATEmptyStr _) = ATEmptyStr
remdoc x = x

cons :: ArgType -> ArgType -> ArgType
cons x y = cons' (remdoc x) (remdoc y) where
  cons' ATEmptyStr x = x
  cons' x ATEmptyStr = x
  cons' ATFail x = ATFail
  cons' x ATFail = ATFail
  cons' (ATSeq a) (ATSeq b) = ATSeq (a ++ b)
  cons' (ATSeq a) b = ATSeq (a ++ [b])
  cons' a (ATSeq b) = ATSeq (a : b)
  cons' a b = ATSeq [a, b]

atplus :: ArgType -> ArgType -> ArgType
atplus x y = atplus' (remdoc x) (remdoc y) where
  atplus' ATFail x = x
  atplus' x ATFail = x
  atplus' (ATEither a) (ATEither b) = ATEither (a ++ b)
  atplus' (ATEither a) b = ATEither (a ++ [b])
  atplus' a (ATEither b) = ATEither (a : b)
  atplus' a b = ATEither [a, b]

atsum :: [ArgType] -> ArgType
atsum = foldl atplus ATFail

derivative :: ArgType -> Char -> ArgType
derivative ATInt a = ATFail --TODO
derivative ATString a = ATFail --TODO
derivative ATFile a = derivative ATString a --TODO
derivative (ATToken "") a = ATFail
derivative (ATToken (c:"")) a = bool2arg (a == c)
derivative (ATToken (c:cs)) a = if a == c then ATToken cs else ATFail
derivative (ATEither args) a = atsum (map (\arg -> derivative arg a) args)
derivative (ATSet args) a = derivative (ATList (ATEither args)) a --TODO
derivative (ATList arg) a = cons (derivative arg a) (ATList arg)
derivative (ATSeq []) a = ATFail
derivative (ATSeq (t:[])) a = derivative t a
derivative (ATSeq (t:ts)) a = atsum [cons (derivative t a) (ATSeq ts),
                                      cons (bool2arg $ nullable t) (derivative (ATSeq ts) a)]
derivative (ATDocumented arg help) a = ATDocumented (derivative arg a) help
derivative ATEmptyStr _ = ATFail
derivative ATFail _ = ATFail

derivatives :: ArgType -> String -> ArgType
derivatives arg = foldl derivative arg