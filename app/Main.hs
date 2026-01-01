module Main (main) where

import BrainFuck.Bash qualified as Bash
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
    [file] -> fmap Bash.compile (readFile file)
    [] -> error "Provide a .bf file to compile."
    _ -> error "Usage Error."
  putStrLn result
