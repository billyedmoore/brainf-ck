module BrainFuckBash (compile) where

compile :: String -> String
compile s = prelude ++ concatMap handleChar (removeNonBFChars s)

removeNonBFChars :: String -> String
removeNonBFChars = filter (`elem` "<>+-[],.")

prelude :: String
prelude = "#!/bin/bash\nbytes=(0);\ni=0;\n"

handleChar :: Char -> String
handleChar '>' = "((i++));\n"
handleChar '<' = "((i--));\n"
handleChar '+' = "((bytes[i] = (bytes[i] + 1) % 256));\n"
handleChar '-' = "((bytes[i] = (bytes[i] - 1 + 256) % 256));\n"
handleChar '.' = "printf \"\\\\$(printf '%03o' \"${bytes[i]}\")\";\n"
handleChar ',' = "IFS= read -rn 1 -s char && printf '%d' \"'${char:-$\\n}\""
handleChar '[' = "while ((bytes[i] != 0))\ndo\n"
handleChar ']' = "done\n"
handleChar _ = error "Unexpected Char"
