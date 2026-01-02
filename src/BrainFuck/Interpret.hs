module BrainFuck.Interpret (interpret) where

import BrainFuck.Parse (BrainFuckAST (..))
import Data.Char (chr, ord)
import Data.Map qualified as Map
import Data.Word (Word8)

type MachineState = (Int, Map.Map Int Word8)

interpret :: [BrainFuckAST] -> IO ()
interpret ast = do
  _ <- interpretInternal ast startingState
  return ()
  where
    startingState :: MachineState
    startingState = (0, Map.empty)

    interpretInternal :: [BrainFuckAST] -> MachineState -> IO MachineState
    interpretInternal (PtrArithmetic n : xs) (i, tape) = interpretInternal xs (i + n, tape)
    interpretInternal (DataArithmetic n : xs) (i, tape) =
      let wordN = fromIntegral n :: Word8
       in interpretInternal xs (i, Map.insertWith (+) i wordN tape)
    interpretInternal (GetChar : xs) (i, tape) = do
      c <- getChar
      let cAscii = fromIntegral (ord c) :: Word8
      interpretInternal xs (i, Map.insert i cAscii tape)
    interpretInternal (PutChar : xs) (i, tape) = do
      let val = Map.findWithDefault 0 i tape :: Word8
      let c = chr $ fromIntegral val
      putChar c
      interpretInternal xs (i, tape)
    interpretInternal (Loop body : xs) (i, tape) = do
      let val = Map.findWithDefault 0 i tape
      if val == 0
        then
          interpretInternal xs (i, tape)
        else do
          stateAfterBody <- interpretInternal body (i, tape)
          interpretInternal (Loop body : xs) stateAfterBody
    interpretInternal [] state = return state
