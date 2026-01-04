module BrainFuck.Optimize (optimize) where

import BrainFuck.Parse (BrainFuckAST (..))

optimize :: [BrainFuckAST] -> [BrainFuckAST]
optimize = squash

squash :: [BrainFuckAST] -> [BrainFuckAST]
squash (DataArithmetic n1 : DataArithmetic n2 : xs) = if n1 + n2 == 0 then squash xs else squash (DataArithmetic (n1 + n2) : xs)
squash (PtrArithmetic n1 : PtrArithmetic n2 : xs) = if n1 + n2 == 0 then squash xs else squash (PtrArithmetic (n1 + n2) : xs)
squash (Loop body : xs) = Loop (squash body) : squash xs
squash (x : xs) = x : squash xs
squash [] = []
