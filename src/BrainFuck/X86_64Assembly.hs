module BrainFuck.X86_64Assembly (compile) where

import BrainFuck.Parse (BrainFuckAST (..))
import Control.Monad.State

tapeLength :: Int
tapeLength = 30000

compile :: [BrainFuckAST] -> String
compile ast =
  let (bodyLines, _) = runState (compileNodes ast) 0
   in unlines (prelude ++ map ("  " ++) bodyLines ++ postlude)

prelude :: [String]
prelude =
  [ "section  .text",
    "  global _start",
    "",
    "_start:",
    "  sub rsp, " ++ show (tapeLength + 8),
    "  mov qword [rsp], 8"
  ]

postlude :: [String]
postlude =
  map
    ("  " ++)
    [ "add rsp, " ++ show (tapeLength + 8),
      "mov rax, 60",
      "xor rdi, rdi",
      "syscall"
    ]

compileNodes :: [BrainFuckAST] -> State Int [String]
compileNodes nodes = concat <$> mapM handleNode nodes

handleNode :: BrainFuckAST -> State Int [String]
handleNode (PtrArithmetic n) = return ["add qword [rsp], " ++ show n]
handleNode (DataArithmetic n) =
  return
    [ "mov rax, [rsp]",
      "add byte [rsp+rax], " ++ show n
    ]
handleNode GetChar =
  return
    [ "mov r10, [rsp]",
      "lea rsi, [rsp + r10]",
      "mov rax, 0",
      "mov rdi, 0",
      "mov rdx, 1",
      "syscall"
    ]
handleNode PutChar =
  return
    [ "mov r10, [rsp]",
      "lea rsi, [rsp + r10]",
      "mov rax, 1",
      "mov rdi, 1",
      "mov rdx, 1",
      "syscall"
    ]
handleNode (SetCell n) =
  return
    [ "mov rax, [rsp]",
      "mov byte [rsp+rax], " ++ show n
    ]
handleNode (Loop body) = do
  loopI <- get
  modify (+ 1)
  bodyCode <- compileNodes body
  return $
    [ "loopCheck" ++ show loopI ++ ":",
      "  mov r10, [rsp]",
      "  mov al, [rsp + r10]",
      "  test al, al",
      "  jz " ++ "loopEnd" ++ show loopI
    ]
      ++ map ("  " ++) bodyCode
      ++ [ "  jmp loopCheck" ++ show loopI,
           "loopEnd" ++ show loopI ++ ":"
         ]
