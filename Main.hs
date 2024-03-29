{-# LANGUAGE ScopedTypeVariables #-}

module Main where

--import System.Console.Editline.Readline as EL
import System.Console.Readline as EL


import Data.List
-- import Data.String.Utils
import Control.Concurrent

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
  _ <- installHandler sigINT shellIntHandler Nothing
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
          _ <- installHandler sigINT cmdIntHandler Nothing
          return tid'
      putStrLn $ "ThreadId: " ++ (show tid)
      readEvalPrintLoop
                        
{- exception handlers -}
{- Handle command exceptions -}
ueHandler :: IOError -> IO (Either () ThreadId)
ueHandler e     = do 
                    putStrLn $ "Exception raised: " ++ (show e)
                    return $ Left ()

{- Handle the SIGINT interrupt to the shell itself -}
shellIntHandler :: Handler
shellIntHandler = Catch (return ())

{- Handle the SIGINT interrupt to a process -}
cmdIntHandler :: Handler
cmdIntHandler   = Catch (return () >> readEvalPrintLoop)



callbackWrapper :: IO () -> EL.Callback
callbackWrapper f = \_ _ -> f >> return 0

{- 
run a command

It's still pretty ugly right now, the fix is a ShellCommand instance
for sequences of ShellCommands that simply pipes them all together,
but piping isn't the only thing we might want to do with a list
of Commands (we might want to run them all sequentially), so for now
this will have to do.
-}
execCmd :: Command -> IO (Either () ThreadId)
execCmd (Pipeline _       []  _       _) = return $ Left ()
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
execCmd (Pipeline Nothing inv Nothing Bg)  
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

commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix as = if any null as then []
	     	  else case (unify $ map head as) of
		       Nothing -> []
		       Just a -> a : (commonPrefix $ map tail as)

{- tab complete what is currently in the terminal buffer -}
tabComplete :: IO ()
tabComplete = do
  putEmpty
  bufFull <- getLineBuffer
  point <- getPoint
  let buf = take point bufFull
  let tokenized = doTokenizeWithEndWS buf
  let derived = derivatives schema tokenized
  let lasttok = last $ "":tokenized
  putStrLn $ docs Page $ fst.upToWS $ derived
  prefix <- if (requiredFilenameCompletion derived) then
    do {s <- filenameCompletionFunction lasttok;
        putStrLn $ concat $ intersperse "\n" s;
	return $ drop (length lasttok) (commonPrefix s)}
    else return $ requiredNextString derived
  stuffStr prefix
  EL.redisplay
  EL.resetLineState -- readline-only; remove this line if you're using editline;

{- 
main loop, accept input and wait for either tab (complete) or enter (exec) 
-}
main :: IO ()
main = do
  EL.bindKey '!' (\_ _ -> do {return 0}) -- I'm not sure why this is necessary
  EL.addDefun "ss-tab" (callbackWrapper tabComplete) (Just '\t')
  readEvalPrintLoop

testing :: Char -> Char -> Bool
testing ' ' '"' = True
testing _   _   = False
