module BrainFuck.C (compile) where

import BrainFuck.Parse (BrainFuckAST (..))

tapeLength :: Int
tapeLength = 30000

compile :: [BrainFuckAST] -> String
compile ast = unlines (prelude ++ map (replicate 2 ' ' ++) (concatMap handleNode ast) ++ postlude)

prelude :: [String]
prelude = ["#include <stdio.h>", "", "int main(){", "unsigned char tape[" ++ show tapeLength ++ "];", "int i = 0;"]

postlude :: [String]
postlude = ["  return 0;", "}"]

handleNode :: BrainFuckAST -> [String]
handleNode (PtrArithmetic n) = ["i+=" ++ show n ++ ";"]
handleNode (DataArithmetic n) = ["tape[i]+=" ++ show n ++ ";"]
handleNode GetChar = ["tape[i]=getchar();"]
handleNode PutChar = ["putchar(tape[i]);"]
handleNode (SetCell n) = ["tape[i]=" ++ show n ++ ";"]
handleNode (Loop body) = ["while (tape[i] != 0){"] ++ map (replicate 2 ' ' ++) (concatMap handleNode body) ++ ["}"]
