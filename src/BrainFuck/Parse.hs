module BrainFuck.Parse (BrainFuckAST (..), ParseError (..), parse) where

data ParseError
  = UnexpectedSymbol Char
  | UnmatchedLoopOpen
  | UnmatchedLoopClose
  deriving (Eq, Show)

data BrainFuckAST
  = DataIncrement Int
  | DataDecrement Int
  | PtrIncrement Int
  | PtrDecrement Int
  | GetChar
  | PutChar
  | Loop [BrainFuckAST]
  deriving (Eq, Show)

parse :: String -> Either ParseError [BrainFuckAST]
parse = fmap squash . basicParse . removeNonBFChars

removeNonBFChars :: String -> String
removeNonBFChars = filter (`elem` "<>+-[],.")

basicParse :: String -> Either ParseError [BrainFuckAST]
basicParse ('+' : xs) = (DataIncrement 1 :) <$> basicParse xs
basicParse ('-' : xs) = (DataDecrement 1 :) <$> basicParse xs
basicParse ('>' : xs) = (PtrIncrement 1 :) <$> basicParse xs
basicParse ('<' : xs) = (PtrDecrement 1 :) <$> basicParse xs
basicParse ('.' : xs) = (PutChar :) <$> basicParse xs
basicParse (',' : xs) = (GetChar :) <$> basicParse xs
basicParse ('[' : xs) = do
  (loopBody, rest) <- findLoopEnd xs "" 0
  parsedBody <- basicParse loopBody
  parsedRest <- basicParse rest
  return (Loop parsedBody : parsedRest)
  where
    findLoopEnd :: String -> String -> Int -> Either ParseError (String, String)
    findLoopEnd (']' : rest) loopBody 0 = Right (reverse loopBody, rest)
    findLoopEnd (']' : rest) loopBody n = findLoopEnd rest (']' : loopBody) (n - 1)
    findLoopEnd ('[' : rest) loopBody n = findLoopEnd rest ('[' : loopBody) (n + 1)
    findLoopEnd (c : rest) loopBody n = findLoopEnd rest (c : loopBody) n
    findLoopEnd [] _ _ = Left UnmatchedLoopOpen
basicParse (']' : _) = Left UnmatchedLoopClose
basicParse [] = Right []
basicParse (c : _) = Left $ UnexpectedSymbol c

squash :: [BrainFuckAST] -> [BrainFuckAST]
squash (DataIncrement n1 : DataIncrement n2 : xs) = squash (DataIncrement (n1 + n2) : xs)
squash (DataDecrement n1 : DataDecrement n2 : xs) = squash (DataDecrement (n1 + n2) : xs)
squash (PtrIncrement n1 : PtrIncrement n2 : xs) = squash (PtrIncrement (n1 + n2) : xs)
squash (PtrDecrement n1 : PtrDecrement n2 : xs) = squash (PtrDecrement (n1 + n2) : xs)
squash (Loop body : xs) = (Loop (squash body)) : squash xs
squash (x : xs) = x : squash xs
squash [] = []
