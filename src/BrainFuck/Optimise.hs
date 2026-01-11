module BrainFuck.Optimise (optimise) where

import BrainFuck.Parse (BrainFuckAST (..))

-- optimise until no changes are made
optimise :: [BrainFuckAST] -> [BrainFuckAST]
optimise ast =
  let opt = optimiseInternal ast
   in if ast == opt
        then
          opt
        else optimise opt

optimiseInternal :: [BrainFuckAST] -> [BrainFuckAST]
optimiseInternal =
  banishChangeBeforeClobber
    . dataArithmeticToSetCell
    . clearingLoops
    . banishPointlessLoops
    . squash

squash :: [BrainFuckAST] -> [BrainFuckAST]
squash (DataArithmetic n1 : DataArithmetic n2 : xs) = if n1 + n2 == 0 then squash xs else squash (DataArithmetic (n1 + n2) : xs)
squash (PtrArithmetic n1 : PtrArithmetic n2 : xs) = if n1 + n2 == 0 then squash xs else squash (PtrArithmetic (n1 + n2) : xs)
squash (Loop body : xs) = Loop (squash body) : squash xs
squash (x : xs) = x : squash xs
squash [] = []

clearingLoops :: [BrainFuckAST] -> [BrainFuckAST]
clearingLoops (Loop [DataArithmetic (-1)] : xs) = SetCell 0 : clearingLoops xs
clearingLoops (Loop [DataArithmetic (1)] : xs) = SetCell 0 : clearingLoops xs
clearingLoops (Loop body : xs) = Loop (clearingLoops body) : clearingLoops xs
clearingLoops (x : xs) = x : clearingLoops xs
clearingLoops [] = []

banishPointlessLoops :: [BrainFuckAST] -> [BrainFuckAST]
-- Remove loop at start (since tape starts with 0s)
banishPointlessLoops (Loop _ : xs) = banishPointlessLoops xs
banishPointlessLoops (SetCell 0 : xs) = banishPointlessLoops xs
banishPointlessLoops xs = banishBackToBackLoops xs

banishBackToBackLoops :: [BrainFuckAST] -> [BrainFuckAST]
banishBackToBackLoops (Loop body : Loop _ : xs) = banishBackToBackLoops (Loop (banishBackToBackLoops body) : xs)
banishBackToBackLoops (Loop body : xs) = Loop (banishBackToBackLoops body) : xs
banishBackToBackLoops (x : xs) = x : banishBackToBackLoops xs
banishBackToBackLoops [] = []

banishChangeBeforeClobber :: [BrainFuckAST] -> [BrainFuckAST]
-- Loops are not involved in the clobbers
banishChangeBeforeClobber (Loop body : xs) = Loop (banishChangeBeforeClobber body) : banishChangeBeforeClobber xs
banishChangeBeforeClobber [] = []
banishChangeBeforeClobber [x] = [x]
banishChangeBeforeClobber (prev : inst : xs)
  | doesClobber inst && isRemovable prev = banishChangeBeforeClobber (inst : xs)
  | otherwise = prev : banishChangeBeforeClobber (inst : xs)
  where
    doesClobber :: BrainFuckAST -> Bool
    doesClobber (DataArithmetic _) = True
    doesClobber (SetCell _) = True
    doesClobber GetChar = True
    -- Loop does clear the cell eventually but relies on the cells value
    doesClobber (Loop _) = False
    doesClobber _ = False

    isRemovable :: BrainFuckAST -> Bool
    isRemovable (DataArithmetic _) = True
    isRemovable (SetCell _) = True
    isRemovable _ = False

-- If the first elem is a dataArithmetic, convert to SetCell
-- Then call arithmeticAfterZeroToSetCell to convert all other
-- DataArithmetic on known zero cells to SetCell
dataArithmeticToSetCell :: [BrainFuckAST] -> [BrainFuckAST]
dataArithmeticToSetCell (DataArithmetic n : xs) = SetCell (fromIntegral n) : arithmeticAfterZeroToSetCell xs
dataArithmeticToSetCell xs = arithmeticAfterZeroToSetCell xs

arithmeticAfterZeroToSetCell :: [BrainFuckAST] -> [BrainFuckAST]
-- DataArithmetic after ClearCell (i.e. SetCell 0) can be replaced with single SetCell
arithmeticAfterZeroToSetCell ((SetCell 0) : DataArithmetic n : xs) =
  SetCell (fromIntegral n) : arithmeticAfterZeroToSetCell xs
-- DataArithmetic after loop can be replaced with a SetCell i.e x += n -> x = n
arithmeticAfterZeroToSetCell (Loop body : DataArithmetic n : xs) =
  Loop (arithmeticAfterZeroToSetCell body) : SetCell (fromIntegral n) : arithmeticAfterZeroToSetCell xs
arithmeticAfterZeroToSetCell (Loop body : xs) =
  Loop (arithmeticAfterZeroToSetCell body) : arithmeticAfterZeroToSetCell xs
arithmeticAfterZeroToSetCell (x : xs) = x : arithmeticAfterZeroToSetCell xs
arithmeticAfterZeroToSetCell [] = []
