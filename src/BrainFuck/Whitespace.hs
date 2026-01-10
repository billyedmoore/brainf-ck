module BrainFuck.Whitespace (compile) where

import BrainFuck.Parse (BrainFuckAST (..))
import Control.Monad.State
import Data.Char (chr)

space, lineFeed, tab :: Char
space = chr 32
lineFeed = chr 10
tab = chr 9

heapRetrieveCmd :: String
heapRetrieveCmd = [tab, tab, tab]

heapStoreCmd :: String
heapStoreCmd = [tab, tab, space]

stackPushCmd :: Int -> String
stackPushCmd n = [space, space] ++ encodeNumber n

stackDuplicateCmd :: String
stackDuplicateCmd = [space, lineFeed, space]

addCmd :: String
addCmd = [tab, space, space, space]

readCharCmd :: String
readCharCmd = [tab, lineFeed, tab, space]

writeCharCmd :: String
writeCharCmd = [tab, lineFeed, space, space]

endProgramCmd :: String
endProgramCmd = [lineFeed, lineFeed, lineFeed]

jumpToLoopStartCmd :: Int -> String
jumpToLoopStartCmd i = [lineFeed, space, lineFeed] ++ replicate i tab ++ [lineFeed]

jumpToLoopEndIfZeroCmd :: Int -> String
jumpToLoopEndIfZeroCmd i = [lineFeed, tab, space] ++ replicate i space ++ [lineFeed]

loopStartLabelCmd :: Int -> String
loopStartLabelCmd i = [lineFeed, space, space] ++ replicate i tab ++ [lineFeed]

loopEndLabelCmd :: Int -> String
loopEndLabelCmd i = [lineFeed, space, space] ++ replicate i space ++ [lineFeed]

encodeNumber :: Int -> String
encodeNumber n = (if n >= 0 then space else tab) : bin (abs n) ++ [lineFeed]
  where
    bin 0 = [space]
    bin x = bin (x `div` 2) ++ [if even x then space else tab]

compile :: [BrainFuckAST] -> String
compile ast =
  let (bodyLines, _) = runState (compileNodes ast) 1
   in concat ([prelude] ++ bodyLines ++ [postlude])

prelude :: String
prelude = [space, space] ++ encodeNumber 0

postlude :: String
postlude = endProgramCmd

compileNodes :: [BrainFuckAST] -> State Int [String]
compileNodes nodes = concat <$> mapM handleNode nodes

handleNode :: BrainFuckAST -> State Int [String]
handleNode (PtrArithmetic n) = return [stackPushCmd n, addCmd]
handleNode (DataArithmetic n) =
  return [stackDuplicateCmd, stackDuplicateCmd, heapRetrieveCmd, stackPushCmd n, addCmd, heapStoreCmd]
handleNode GetChar = return [stackDuplicateCmd, readCharCmd]
handleNode PutChar = return [stackDuplicateCmd, heapRetrieveCmd, writeCharCmd]
handleNode (SetCell n) = return [stackDuplicateCmd, stackPushCmd (fromIntegral n), heapStoreCmd]
handleNode (Loop body) = do
  loopId <- get
  modify (+ 1)
  bodyCode <- compileNodes body
  return $
    [ loopStartLabelCmd loopId,
      stackDuplicateCmd,
      heapRetrieveCmd,
      jumpToLoopEndIfZeroCmd loopId
    ]
      ++ bodyCode
      ++ [ jumpToLoopStartCmd loopId,
           loopEndLabelCmd loopId
         ]
