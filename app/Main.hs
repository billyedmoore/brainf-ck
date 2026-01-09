module Main (main) where

import BrainFuck (parseAndOptimise)
import BrainFuck.Bash qualified as Bash
import BrainFuck.C qualified as C
import BrainFuck.Haskell qualified as Haskell
import BrainFuck.Interpret qualified as Interpret
import BrainFuck.LLVM qualified as LLVM
import BrainFuck.Pascal qualified as Pascal
import BrainFuck.Whitespace qualified as WS
import BrainFuck.X86_64Assembly qualified as X86_64Asm
import BrainFuck.X86_64MachineCode qualified as X86_64MC
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Options.Applicative
import System.Exit (die)

takeExtension :: String -> String
takeExtension s = takeExtensionInternal (reverse s) ""
  where
    takeExtensionInternal :: String -> String -> String
    takeExtensionInternal ('.' : _) acc = acc
    takeExtensionInternal (x : xs) acc = takeExtensionInternal xs (x : acc)
    takeExtensionInternal [] _ = ""

writeBytes :: FilePath -> [Word8] -> IO ()
writeBytes path bytes = BS.writeFile path (BS.pack bytes)

data Options = Options
  { _inputFile :: String,
    _outputFile :: Maybe String
  }

optionParser :: Parser Options
optionParser =
  Options
    <$> strArgument
      ( metavar "INPUT_FILE"
          <> help "Input BrainFuck file."
      )
    <*> optional
      ( strOption
          ( long "output"
              <> metavar "OUTPUT_FILE"
              <> short 'o'
              <> help
                ( "Output file, language of transpilation will be decided by file extension."
                    <> " If omitted the input file will be interpretted directly."
                )
          )
      )

main :: IO ()
main = programMain =<< execParser opts
  where
    opts =
      info
        (optionParser <**> helper)
        ( fullDesc
            <> progDesc "Transpile or interpret BrainFuck programs."
            <> header "brainf-ck - a BrainFuck interpreter and transpiler."
        )

programMain :: Options -> IO ()
programMain (Options inputFile maybeOutputFile) = do
  content <- readFile inputFile
  case parseAndOptimise content of
    Left err ->
      die $ "Parse Error: " ++ show err
    Right ast ->
      case maybeOutputFile of
        (Just outputFile) -> case takeExtension outputFile of
          s | s `elem` ["sh", "bash"] -> writeFile outputFile (Bash.compile ast)
          s | s == "c" -> writeFile outputFile (C.compile ast)
          s | s == "ws" -> writeFile outputFile (WS.compile ast)
          s | s == "ll" -> writeFile outputFile (LLVM.compile ast)
          s | s == "hs" -> writeFile outputFile (Haskell.compile ast)
          s | s == "pas" -> writeFile outputFile (Pascal.compile ast)
          s | s == "asm" -> writeFile outputFile (X86_64Asm.compile ast)
          s | s == "out" -> writeBytes outputFile (X86_64MC.compile ast)
          _ -> die $ "Unsupported File Type: " ++ show (takeExtension outputFile)
        Nothing -> Interpret.interpret ast
