{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.String.Utils

import System.Console.Readline as EL
import CmdParser
import Command

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
  maybeLine <- EL.readline ">> "
  case maybeLine of 
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line -> do EL.addHistory line
                    execCmd $ Pipeline Nothing (toInv line) Nothing Fg
                    readEvalPrintLoop

toInv :: String -> [Invocation]
toInv line = [Invocation cmd args]
  where
    (_:cmd:args) = splitBy (\x -> (x == '"') || (x == ' ')) (show line)

callbackWrapper :: IO () -> EL.Callback
callbackWrapper f = \_ _ -> f >> return 0

putEmpty :: IO ()
putEmpty = putStrLn ""

stuffStr :: String -> IO ()
stuffStr s = mapM (EL.stuffChar) s >> return ()

tabComplete :: IO ()
tabComplete = do
  putEmpty
  bufFull <- getLineBuffer
  point <- getPoint
  let buf = take point bufFull
  putStrLn $ "Help for the function '" ++ buf ++ "'."
  putEmpty
  stuffStr "[]"
  putStrLn "completion 1\tcompletion 2"
  EL.redisplay

main :: IO ()
main = do
  EL.bindKey '!' (\i c -> do {return 0}) -- I'm not sure why this is necessary
  EL.addDefun "ss-tab" (callbackWrapper tabComplete) (Just '\t')
  readEvalPrintLoop

testing :: Char -> Char -> Bool
testing ' ' '"' = True
testing _   _   = False
