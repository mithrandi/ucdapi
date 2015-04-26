{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Application (makeFoundation, db)
import Control.Monad.Logger (runStderrLoggingT)
import Import
import Load (loadUCD)
import System.Exit (exitWith, ExitCode(ExitFailure))

main :: IO ()
main = do
  args <- getArgs
  case args of
   [inputFileName] -> db $ loadUCD inputFileName
   otherwise -> do
     hPutStrLn stderr "Usage: loaddata <UCD flat XML file>"
     exitWith (ExitFailure 1)
