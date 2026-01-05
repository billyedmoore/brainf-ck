module BrainFuck (BrainFuckAST (..), ParseError (..), parse, parseAndOptimise) where

import BrainFuck.Optimise (optimise)
import BrainFuck.Parse (BrainFuckAST (..), ParseError (..), parse)

parseAndOptimise :: String -> Either ParseError [BrainFuckAST]
parseAndOptimise = fmap optimise . parse
