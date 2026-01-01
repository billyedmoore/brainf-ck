module BrainFuck.Parse (AST (..), parse) where

data AST
  = DataIncrement Int
  | DataDecrement Int
  | PtrIncrement Int
  | PtrDecrement Int
  | GetChar
  | PutChar
  | Loop [AST]
  deriving (Eq, Show)

parse :: String -> [AST]
parse = squash . basicParse . removeNonBFChars

removeNonBFChars :: String -> String
removeNonBFChars = filter (`elem` "<>+-[],.")

basicParse :: String -> [AST]
basicParse ('+' : xs) = DataIncrement 1 : basicParse xs
basicParse ('-' : xs) = DataDecrement 1 : basicParse xs
basicParse ('>' : xs) = PtrIncrement 1 : basicParse xs
basicParse ('<' : xs) = PtrDecrement 1 : basicParse xs
basicParse ('.' : xs) = PutChar : basicParse xs
basicParse (',' : xs) = GetChar : basicParse xs
basicParse ('[' : xs) =
  let (loopBody, rest) = findLoopEnd xs "" 0
   in Loop (basicParse loopBody) : basicParse rest
  where
    findLoopEnd :: String -> String -> Int -> (String, String)
    findLoopEnd (']' : rest) loopBody 0 = (reverse loopBody, rest)
    findLoopEnd (']' : rest) loopBody n = findLoopEnd rest (']' : loopBody) (n - 1)
    findLoopEnd ('[' : rest) loopBody n = findLoopEnd rest ('[' : loopBody) (n + 1)
    findLoopEnd (c : rest) loopBody n = findLoopEnd rest (c : loopBody) n
    findLoopEnd [] _ _ = error "Loop does not end."
basicParse (']' : _) = error "Loop ends before it begins."
basicParse [] = []
basicParse (c : _) = error ("Unexpected symbol - " ++ show c ++ ".")

squash :: [AST] -> [AST]
squash (DataIncrement n1 : DataIncrement n2 : xs) = squash (DataIncrement (n1 + n2) : xs)
squash (DataDecrement n1 : DataDecrement n2 : xs) = squash (DataDecrement (n1 + n2) : xs)
squash (PtrIncrement n1 : PtrIncrement n2 : xs) = squash (PtrIncrement (n1 + n2) : xs)
squash (PtrDecrement n1 : PtrDecrement n2 : xs) = squash (PtrDecrement (n1 + n2) : xs)
squash (a : xs) = a : squash xs
squash [] = []
