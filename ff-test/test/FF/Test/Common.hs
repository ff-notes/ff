module FF.Test.Common (diffCmd) where

diffCmd :: String -> String -> [String]
diffCmd ref new = ["colordiff", "-bu", ref, new]
