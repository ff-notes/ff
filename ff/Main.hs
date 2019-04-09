module Main where

import           FF.CLI (cli)

import           Paths_ff (version)

main :: IO ()
main = cli version
