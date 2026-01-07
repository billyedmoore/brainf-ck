module BrainFuck.Haskell (compile) where

import BrainFuck.Parse (BrainFuckAST (..))
import Data.List (intercalate)

compile :: [BrainFuckAST] -> String
compile ast = unlines (prelude ++ ["  let fs = " ++ formatList (map handleNode ast)] ++ postlude)

prelude :: [String]
prelude =
  [ "#!/usr/bin/env stack",
    "{- stack script --resolver lts-22.0 --package containers -}",
    "",
    "{- cabal:",
    "build-depends: base, containers",
    "-}",
    "",
    "import Control.Monad (foldM, foldM_)",
    "import Data.Char (chr, ord)",
    "import Data.Map qualified as Map",
    "import Data.Word (Word8)",
    "",
    "type MachineState = (Int, Map.Map Int Word8)",
    "",
    "clearCell :: MachineState -> IO MachineState",
    "clearCell (i, tape) = return (i, Map.delete i tape)",
    "",
    "ptrArithmetic :: Int -> MachineState -> IO MachineState",
    "ptrArithmetic n (i, tape) = return (n + i, tape)",
    "",
    "dataArithmetic :: Int -> MachineState -> IO MachineState",
    "dataArithmetic n (i, tape) =",
    "  let n8 = fromIntegral n :: Word8",
    "   in return (i, Map.insertWith (+) i n8 tape)",
    "",
    "while :: [MachineState -> IO MachineState] -> MachineState -> IO MachineState",
    "while body (i, tape) = do",
    "  if (Map.findWithDefault 0 i tape) == 0 then return (i, tape)",
    "  else do",
    "   nextState <- foldM (\\state f -> f state) (i, tape) body",
    "   while body nextState",
    "",
    "writeChar :: MachineState -> IO MachineState",
    "writeChar (i, tape) = do",
    "  let val = Map.findWithDefault 0 i tape :: Word8",
    "  let c = chr $ fromIntegral val",
    "  putChar c",
    "  return (i, tape)",
    "",
    "readChar :: MachineState -> IO MachineState",
    "readChar (i, tape) = do",
    "  c <- getChar",
    "  let cAscii = fromIntegral (ord c) :: Word8",
    "  return (i, Map.insert i cAscii tape)",
    "",
    "main :: IO ()",
    "main = do"
  ]

postlude :: [String]
postlude = ["  foldM_ (\\x f -> f x) (0, Map.empty) fs"]

formatList :: [String] -> String
formatList xs = "[" ++ intercalate ", " xs ++ "]"

handleNode :: BrainFuckAST -> String
handleNode (PtrArithmetic n) = "ptrArithmetic (" ++ show n ++ ")"
handleNode (DataArithmetic n) = "dataArithmetic (" ++ show n ++ ")"
handleNode GetChar = "readChar"
handleNode PutChar = "writeChar"
handleNode ClearCell = "clearCell"
handleNode (Loop body) = "while " ++ formatList (map handleNode body)
