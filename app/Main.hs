module Main (main) where

import BrainFuck.Bash qualified as Bash
import BrainFuck.Interpret qualified as Interpret
import BrainFuck.Parse (parse)
import Options.Applicative
import System.Exit (die)

takeExtension :: String -> String
takeExtension s = takeExtensionInternal (reverse s) ""
  where
    takeExtensionInternal :: String -> String -> String
    takeExtensionInternal ('.' : _) acc = acc
    takeExtensionInternal (x : xs) acc = takeExtensionInternal xs (x : acc)
    takeExtensionInternal [] _ = ""

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
  case parse content of
    Left err ->
      die $ "Parse Error: " ++ show err
    Right ast ->
      case maybeOutputFile of
        (Just outputFile) -> case takeExtension outputFile of
          s | s `elem` ["sh", "bash"] -> writeFile outputFile (Bash.compile ast)
          _ -> die $ "Unsupported File Type: " ++ show (takeExtension outputFile)
        Nothing -> Interpret.interpret ast
