module Main where

import Debug.Trace (traceM)

import FF.CLI (cli)

import Paths_ff (version)

main :: IO ()
main = do
  traceM "main"
  cli version
