module BrainFuck.Parse (BrainFuckAST (..), ParseError (..), parse) where

data ParseError
  = UnexpectedSymbol Char
  | UnmatchedLoopOpen
  | UnmatchedLoopClose
  deriving (Eq, Show)

data BrainFuckAST
  = DataArithmetic Int
  | PtrArithmetic Int
  | GetChar
  | PutChar
  | Loop [BrainFuckAST]
  | ClearCell
  deriving (Eq, Show)

parse :: String -> Either ParseError [BrainFuckAST]
parse = basicParse . removeNonBFChars

removeNonBFChars :: String -> String
removeNonBFChars = filter (`elem` "<>+-[],.")

basicParse :: String -> Either ParseError [BrainFuckAST]
basicParse ('+' : xs) = (DataArithmetic 1 :) <$> basicParse xs
basicParse ('-' : xs) = (DataArithmetic (-1) :) <$> basicParse xs
basicParse ('>' : xs) = (PtrArithmetic 1 :) <$> basicParse xs
basicParse ('<' : xs) = (PtrArithmetic (-1) :) <$> basicParse xs
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
