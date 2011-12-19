module Main where

import Data.List
import Data.String.Utils

import System.Console.Editline.Readline as EL

readEvalPrintLoop :: IO ()
readEvalPrintLoop = do
  maybeLine <- EL.readline ">> "
  case maybeLine of 
    Nothing     -> return ()
    Just "exit" -> return ()
    Just line -> do EL.addHistory line
                    putStrLn $ "input: " ++ (show line)
                    readEvalPrintLoop

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