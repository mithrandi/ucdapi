{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Application (db)
import Data.Maybe (fromJust)
import Import
import Load (loadUCD)
import System.Exit (exitWith, ExitCode(ExitFailure))

main :: IO ()
main = do
  args <- getArgs
  case args of
   [ver, inputFileName] -> db $ loadUCD (fromJust $ fromPathPiece ver) inputFileName
   _ -> do
     hPutStrLn stderr "Usage: loaddata <UCD version> <UCD flat XML file>"
     exitWith (ExitFailure 1)
