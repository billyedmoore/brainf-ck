module BrainFuck.Bash (compile) where

import BrainFuck.Parse (BrainFuckAST (..))

compile :: [BrainFuckAST] -> String
compile ast = unlines (prelude ++ concatMap handleNode ast)

prelude :: [String]
prelude = ["#!/bin/bash", "bytes=(0);", "i=0;"]

handleNode :: BrainFuckAST -> [String]
handleNode (PtrArithmetic n) = ["((i+=" ++ show n ++ "))"]
handleNode (DataArithmetic n) = ["((bytes[i] = (bytes[i] + " ++ show n ++ " + 256) % 256));"]
handleNode GetChar = ["read -rn 1 char && { printf -v \"bytes[$i]\" \"%d\" \"'${char}\"; }"]
handleNode PutChar = ["printf \"\\\\$(printf '%03o' \"${bytes[i]}\")\";"]
handleNode (Loop body) = ["while ((bytes[i] != 0))", "do"] ++ map (replicate 2 ' ' ++) (concatMap handleNode body) ++ ["done"]
