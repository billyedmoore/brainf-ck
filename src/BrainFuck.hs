module BrainFuck (BrainFuckAST (..), ParseError (..), parse, parseAndOptimize) where

import BrainFuck.Optimize (optimize)
import BrainFuck.Parse (BrainFuckAST (..), ParseError (..), parse)

parseAndOptimize :: String -> Either ParseError [BrainFuckAST]
parseAndOptimize = fmap optimize . parse
