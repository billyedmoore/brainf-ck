module BrainFuck.Optimize (optimize) where

import BrainFuck.Parse (BrainFuckAST (..))

optimize :: [BrainFuckAST] -> [BrainFuckAST]
optimize = clearingLoops . banishPointlessLoops . squash

squash :: [BrainFuckAST] -> [BrainFuckAST]
squash (DataArithmetic n1 : DataArithmetic n2 : xs) = if n1 + n2 == 0 then squash xs else squash (DataArithmetic (n1 + n2) : xs)
squash (PtrArithmetic n1 : PtrArithmetic n2 : xs) = if n1 + n2 == 0 then squash xs else squash (PtrArithmetic (n1 + n2) : xs)
squash (Loop body : xs) = Loop (squash body) : squash xs
squash (x : xs) = x : squash xs
squash [] = []

clearingLoops :: [BrainFuckAST] -> [BrainFuckAST]
clearingLoops (Loop [DataArithmetic (-1)] : xs) = ClearCell : clearingLoops xs
clearingLoops (Loop [DataArithmetic (1)] : xs) = ClearCell : clearingLoops xs
clearingLoops (Loop body : xs) = Loop (clearingLoops body) : clearingLoops xs
clearingLoops (x : xs) = x : clearingLoops xs
clearingLoops [] = []

banishPointlessLoops :: [BrainFuckAST] -> [BrainFuckAST]
banishPointlessLoops (Loop _ : xs) = banishPointlessLoops xs
banishPointlessLoops xs = banishBackToBackLoops xs

banishBackToBackLoops :: [BrainFuckAST] -> [BrainFuckAST]
banishBackToBackLoops (Loop body : Loop _ : xs) = banishBackToBackLoops (Loop (banishBackToBackLoops body) : xs)
banishBackToBackLoops (Loop body : xs) = Loop (banishBackToBackLoops body) : xs
banishBackToBackLoops (x:xs) = x : banishBackToBackLoops xs
banishBackToBackLoops [] = []
