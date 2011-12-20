{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List
import Data.String.Utils
import Control.Concurrent

import System.Console.Readline as EL
import System.Posix.Signals
import CmdParser
import Command
import HSH
import Completion
import Derivative
import Schema


readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
  installHandler sigINT shellIntHandler Nothing
  maybeLine <- EL.readline ">> "
  case maybeLine of 
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line -> do EL.addHistory line
                    eitherTid <- (execCmd $ buildCmd line) `catch` ueHandler 
                    tid <- case eitherTid of
                      Left () -> myThreadId
                      Right tid' -> do 
                                      installHandler sigINT cmdIntHandler Nothing
                                      return tid'
                    putStrLn $ "ThreadId: " ++ (show tid)
                    readEvalPrintLoop
                        

ueHandler e = do {putStrLn "unrecognized command"; return $ Left ()}
shellIntHandler = Catch (return ())
cmdIntHandler = Catch (return () >> readEvalPrintLoop)

callbackWrapper :: IO () -> EL.Callback
callbackWrapper f = \_ _ -> f >> return 0

{- 
run a command
-}
execCmd :: Command -> IO (Either () ThreadId)
execCmd (Pipeline redirIn [] redirOut fgbg) = return $ Left ()
execCmd (Pipeline redirIn inv redirOut Fg) = do 
                                               runIO inv;
                                               tid <- myThreadId
                                               return $ Right tid
execCmd (Pipeline redirIn inv redirOut Bg) = do
                                               tid <- forkIO (do {runIO inv})
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
