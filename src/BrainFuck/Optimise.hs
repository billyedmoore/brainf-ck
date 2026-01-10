module BrainFuck.Optimise (optimise) where

import BrainFuck.Parse (BrainFuckAST (..))

optimise :: [BrainFuckAST] -> [BrainFuckAST]
optimise = banishBackToBackSetCell . dataArithmeticToSetCell . clearingLoops . banishPointlessLoops . squash

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
banishPointlessLoops (Loop _ : xs) = banishPointlessLoops xs
banishPointlessLoops xs = banishBackToBackLoops xs

banishBackToBackLoops :: [BrainFuckAST] -> [BrainFuckAST]
banishBackToBackLoops (Loop body : Loop _ : xs) = banishBackToBackLoops (Loop (banishBackToBackLoops body) : xs)
banishBackToBackLoops (Loop body : xs) = Loop (banishBackToBackLoops body) : xs
banishBackToBackLoops (x : xs) = x : banishBackToBackLoops xs
banishBackToBackLoops [] = []

banishBackToBackSetCell :: [BrainFuckAST] -> [BrainFuckAST]
banishBackToBackSetCell (SetCell _ : SetCell n : xs) = SetCell n : xs
banishBackToBackSetCell (Loop body : xs) = Loop (banishBackToBackSetCell body) : xs
banishBackToBackSetCell (x : xs) = x : banishBackToBackSetCell xs
banishBackToBackSetCell [] = []

dataArithmeticToSetCell :: [BrainFuckAST] -> [BrainFuckAST]
-- DataArithmetic after ClearCell (i.e. SetCell 0) can be replaced with single SetCell
dataArithmeticToSetCell ((SetCell 0) : DataArithmetic n : xs) =
  SetCell (fromIntegral n) : dataArithmeticToSetCell xs
-- DataArithmetic after loop can be replaced with a SetCell i.e x += n -> x = n
dataArithmeticToSetCell (Loop body : DataArithmetic n : xs) =
  Loop (dataArithmeticToSetCell body) : SetCell (fromIntegral n) : dataArithmeticToSetCell xs
dataArithmeticToSetCell (Loop body : xs) =
  Loop (dataArithmeticToSetCell body) : dataArithmeticToSetCell xs
dataArithmeticToSetCell (x : xs) = x : dataArithmeticToSetCell xs
dataArithmeticToSetCell [] = []
