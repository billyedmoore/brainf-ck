module Main (main) where

import BrainFuck.Bash qualified as Bash
import BrainFuck.Parse (parse)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs

  case args of
    [filePath] -> do
      content <- readFile filePath

      case parse content of
        Left err ->
          die $ "Parse Error: " ++ show err
        Right ast -> do
          let result = Bash.compile ast
          putStrLn result
    _ -> putStrLn "Usage: brainf-ck <bf-file>"
