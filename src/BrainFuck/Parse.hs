module BrainFuck.Parse (AST (..)) where

data AST
  = DataIncrement Int
  | DataDecrement Int
  | PtrIncrement Int
  | PtrDecrement Int
  | GetChar
  | PutChar
  | Loop [AST]
