module BrainFuck.Pascal (compile) where

import BrainFuck.Parse (BrainFuckAST (..))

tapeLength :: Int
tapeLength = 30000

compile :: [BrainFuckAST] -> String
compile ast = unlines (prelude ++ map (replicate 2 ' ' ++) (concatMap handleNode ast) ++ postlude)

prelude :: [String]
prelude =
  [ "program BFInPascal;",
    "uses crt;",
    "",
    "var",
    "  tape: array[0.." ++ show tapeLength ++ "] of byte;",
    "  i: integer;",
    "",
    "begin",
    "  fillchar(tape,sizeof(tape),0);",
    "  i:=0;"
  ]

postlude :: [String]
postlude = ["end."]

handleNode :: BrainFuckAST -> [String]
handleNode (PtrArithmetic n)
  -- Using dec is the same as just inc but free pascal compiler
  -- gives a warning if inc is used with negative numbers.
  | n < 0 = ["dec(i," ++ show (abs n) ++ ");"]
  | otherwise = ["inc(i," ++ show n ++ ");"]
handleNode (DataArithmetic n)
  | n < 0 = ["dec(tape[i]," ++ show (abs n) ++ ");"]
  | otherwise = ["inc(tape[i]," ++ show n ++ ");"]
handleNode GetChar = ["tape[i]:=ord(readkey);"]
handleNode PutChar = ["write(char(tape[i]));"]
handleNode (SetCell n) = ["tape[i]:=" ++ show n ++ ";"]
handleNode (Loop body) =
  ["while tape[i] <> 0 do", "begin"]
    ++ map (replicate 2 ' ' ++) (concatMap handleNode body)
    ++ ["end;"]
