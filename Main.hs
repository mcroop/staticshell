{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.String.Utils
import Control.Concurrent

--import System.Console.Editline.Readline as EL
import System.Console.Readline as EL
import System.Posix.Signals
import CmdParser
import Command
import HSH
import Completion
import Derivative
import Schema

{-
Issue a prompt and respond to input.

Shell commands will be executed as long as they are valid
Basic fg/bg is included (threads can either be fg (they control
terminal input until they exit) or bg (they can write to the
terminal but do not monopolize input
SIGINT will kill all running jobs but will not kill the shell
"exit" will quit the shell
-}
readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
  installHandler sigINT shellIntHandler Nothing
  maybeLine <- EL.readline ">> "
  case maybeLine of 
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line -> do 
      EL.addHistory line
      eitherTid <- (execCmd $ buildCmd line) `catch` ueHandler 
      tid <- case eitherTid of
        Left () -> myThreadId
        Right tid' -> do 
          installHandler sigINT cmdIntHandler Nothing
          return tid'
      putStrLn $ "ThreadId: " ++ (show tid)
      readEvalPrintLoop
                        
{- exception handlers -}
ueHandler e     = do {putStrLn "unrecognized command"; return $ Left ()}
shellIntHandler = Catch (return ())
cmdIntHandler   = Catch (return () >> readEvalPrintLoop)

callbackWrapper :: IO () -> EL.Callback
callbackWrapper f = \_ _ -> f >> return 0

{- 
run a command

it's still pretty ugly right now, the fix is a ShellCommand instance
for sequences of ShellCommands that simply pipes them all together
-}
execCmd :: Command -> IO (Either () ThreadId)
execCmd (Pipeline redirIn [] redirOut fgbg) = return $ Left ()
execCmd (Pipeline Nothing inv Nothing Fg)  
  = do 
      runIO inv
      tid <- myThreadId
      return $ Right tid
execCmd (Pipeline (Just i) inv Nothing Fg) 
  = do
      runIO $ ("cat", [i]) -|- inv
      tid <- myThreadId
      return $ Right tid
execCmd (Pipeline Nothing inv (Just o) Fg) 
  = do
      runIO $ inv -|- (catTo o)
      tid <- myThreadId
      return $ Right tid
execCmd (Pipeline (Just i) inv (Just o) Fg) 
  = do
      runIO $ ("cat", [i]) -|- inv -|- (catTo o)
      tid <- myThreadId
      return $ Right tid
execCmd (Pipeline redirIn inv redirOut Bg)  
  = do
      tid <- forkIO (do {runIO inv})
      return $ Right tid
execCmd (Pipeline (Just i) inv Nothing Bg) 
  = do
      tid <- forkIO (do {runIO $ ("cat", [i]) -|- inv})
      return $ Right tid
execCmd (Pipeline Nothing inv (Just o) Bg) 
  = do
      tid <- forkIO (do {runIO $ inv -|- (catTo o)})
      return $ Right tid
execCmd (Pipeline (Just i) inv (Just o) Bg) 
  = do
      tid <- forkIO (do {runIO $ ("cat", [i]) -|- inv -|- (catTo o)})
      return $ Right tid      
      
putEmpty :: IO ()
putEmpty = putStrLn ""

stuffStr :: String -> IO ()
stuffStr s = mapM (EL.stuffChar) s >> return ()

doTokenizeWithEndWS :: String -> [String]
doTokenizeWithEndWS buf = reverse $ case reverse (doTokenize (buf ++ "!")) of
  [] -> []
  (h : t) -> (init h) : t


tabComplete :: IO ()
tabComplete = do
  putEmpty
  bufFull <- getLineBuffer
  point <- getPoint
  let buf = take point bufFull
  --stuffStr "[]"
  putStrLn $ docs Page $ fst.upToWS $ derivatives schema $ doTokenizeWithEndWS buf
  EL.redisplay

main :: IO ()
main = do
  EL.bindKey '!' (\i c -> do {return 0}) -- I'm not sure why this is necessary
  EL.addDefun "ss-tab" (callbackWrapper tabComplete) (Just '\t')
  readEvalPrintLoop

testing :: Char -> Char -> Bool
testing ' ' '"' = True
testing _   _   = False
