module Main where

import BrainFuck.Parse (AST (..), parse)
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

-- These tests are largely AI written

tests :: TestTree
tests =
  testGroup
    "Brainfuck.Parse Unit Tests"
    [ basicInstructions,
      collapsingLogic,
      loopTests,
      sanitizationTests
    ]

basicInstructions :: TestTree
basicInstructions =
  testGroup
    "Basic Instructions"
    [ testCase "Parse single DataIncrement" $
        parse "+" @?= [DataIncrement 1],
      testCase "Parse single DataDecrement" $
        parse "-" @?= [DataDecrement 1],
      testCase "Parse single PtrIncrement" $
        parse ">" @?= [PtrIncrement 1],
      testCase "Parse single PtrDecrement" $
        parse "<" @?= [PtrDecrement 1],
      testCase "Parse IO tokens" $
        parse ".," @?= [PutChar, GetChar]
    ]

collapsingLogic :: TestTree
collapsingLogic =
  testGroup
    "Collapsing Logic"
    [ testCase "Collapse multiple pluses" $
        parse "+++" @?= [DataIncrement 3],
      testCase "Collapse multiple minuses" $
        parse "----" @?= [DataDecrement 4],
      testCase "Collapse pointer moves" $
        parse ">>>" @?= [PtrIncrement 3],
      testCase "Do not collapse different tokens" $
        parse "+>" @?= [DataIncrement 1, PtrIncrement 1]
    ]

loopTests :: TestTree
loopTests =
  testGroup
    "Loop and Nesting"
    [ testCase "Empty loop" $
        parse "[]" @?= [Loop []],
      testCase "Simple loop body" $
        parse "[+]" @?= [Loop [DataIncrement 1]],
      testCase "Nested loops" $
        parse "[>[-] <]"
          @?= [ Loop
                  [ PtrIncrement 1,
                    Loop [DataDecrement 1],
                    PtrDecrement 1
                  ]
              ],
      testCase "Multiple loops in sequence" $
        parse "[+][-]" @?= [Loop [DataIncrement 1], Loop [DataDecrement 1]]
    ]

sanitizationTests :: TestTree
sanitizationTests =
  testGroup
    "Sanitization (Comments/Whitespace)"
    [ testCase "Ignore letters and spaces" $
        parse "+ + hello >" @?= [DataIncrement 2, PtrIncrement 1],
      testCase "Ignore newlines" $
        parse "+\n+" @?= [DataIncrement 2]
    ]
