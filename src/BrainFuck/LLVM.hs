module BrainFuck.LLVM (compile) where

import BrainFuck.Parse (BrainFuckAST (..))
import Control.Monad.State

compile :: [BrainFuckAST] -> String
compile ast =
  let (bodyLines, _) = runState (compileNodes ast) (0, 0)
   in unlines (prelude ++ map (replicate 2 ' ' ++) bodyLines ++ postlude)

newVariableName :: State (Int, Int) String
newVariableName = do
  (n, x) <- get
  put (n + 1, x)
  return ("v" ++ show n)

newLoopLabelPrefix :: State (Int, Int) String
newLoopLabelPrefix = do
  (x, n) <- get
  put (x, n + 1)
  return ("loop" ++ show n)

tapeLength :: Int
tapeLength = 30000

prelude :: [String]
prelude =
  [ "@tape = global [" ++ show tapeLength ++ " x i8] zeroinitializer, align 1",
    "@i = global i32 0",
    "declare i32 @putchar(i32)",
    "declare i32 @getchar()",
    "",
    "define i32 @main() {"
  ]

postlude :: [String]
postlude = ["  ret i32 0", "}"]

-- Load i from global @i into a local var `%varName`
loadI :: String -> String
loadI varName = "%" ++ varName ++ " = load i32, i32* @i"

-- Load i from global @i into a local var `%varName`
getCellPtr :: String -> String -> String
getCellPtr iVarName varName =
  "%"
    ++ varName
    ++ " = getelementptr inbounds ["
    ++ show tapeLength
    ++ "x i8], ["
    ++ show tapeLength
    ++ " x i8]* @tape, i64 0, i32 %"
    ++ iVarName

compileNodes :: [BrainFuckAST] -> State (Int, Int) [String]
compileNodes nodes = concat <$> mapM handleNode nodes

handleNode :: BrainFuckAST -> State (Int, Int) [String]
handleNode (PtrArithmetic n) = do
  prevI <- newVariableName
  newI <- newVariableName
  return
    [ loadI prevI,
      "%" ++ newI ++ " = add i32 %" ++ prevI ++ ", " ++ show n,
      "store i32 %" ++ newI ++ ", i32* @i"
    ]
handleNode (DataArithmetic n) = do
  localI <- newVariableName
  cellPtr <- newVariableName
  cellVal <- newVariableName
  newCellVal <- newVariableName
  return
    [ loadI localI,
      getCellPtr localI cellPtr,
      "%" ++ cellVal ++ " = load i8, i8* %" ++ cellPtr ++ ", align 1",
      "%" ++ newCellVal ++ " = add i8 %" ++ cellVal ++ ", " ++ show n,
      "store i8 %" ++ newCellVal ++ ", i8* %" ++ cellPtr ++ ", align 1"
    ]
handleNode GetChar = do
  inputVal <- newVariableName
  inputVali8 <- newVariableName
  localI <- newVariableName
  cellPtr <- newVariableName
  return
    [ "%" ++ inputVal ++ " = call i32 @getchar()",
      "%" ++ inputVali8 ++ " = trunc i32 %" ++ inputVal ++ " to i8",
      loadI localI,
      getCellPtr localI cellPtr,
      "store i8 %" ++ inputVali8 ++ ", i8* %" ++ cellPtr ++ ", align 1"
    ]
handleNode PutChar = do
  localI <- newVariableName
  cellPtr <- newVariableName
  cellVal <- newVariableName
  cellVali32 <- newVariableName
  return
    [ loadI localI,
      getCellPtr localI cellPtr,
      "%" ++ cellVal ++ " = load i8, i8* %" ++ cellPtr ++ ", align 1",
      "%" ++ cellVali32 ++ "  = zext i8 %" ++ cellVal ++ " to i32",
      "call i32 @putchar(i32 %" ++ cellVali32 ++ ")"
    ]
handleNode ClearCell = do
  localI <- newVariableName
  cellPtr <- newVariableName
  return
    [ loadI localI,
      getCellPtr localI cellPtr,
      "store i8 0, i8* %" ++ cellPtr ++ ", align 1"
    ]
handleNode (Loop body) = do
  loopId <- newLoopLabelPrefix
  i <- newVariableName
  cellPtr <- newVariableName
  cellVal <- newVariableName
  isCellValZero <- newVariableName
  bodyCode <- compileNodes body

  let loopCheckCondLabelName = loopId ++ "_check-condition"
  let loopBodyLabelName = loopId ++ "_body"
  let loopExitLabelName = loopId ++ "_exit"

  return $
    [ "br label %" ++ loopCheckCondLabelName,
      loopCheckCondLabelName ++ ":",
      loadI i,
      getCellPtr i cellPtr,
      "%" ++ cellVal ++ " = load i8, i8* %" ++ cellPtr ++ ", align 1",
      "%" ++ isCellValZero ++ " = icmp ne i8 %" ++ cellVal ++ ", 0",
      "br i1 %" ++ isCellValZero ++ ", label %" ++ loopBodyLabelName ++ ", label %" ++ loopExitLabelName,
      loopBodyLabelName ++ ":"
    ]
      ++ bodyCode
      ++ [ "br label %" ++ loopCheckCondLabelName,
           loopExitLabelName ++ ":"
         ]
