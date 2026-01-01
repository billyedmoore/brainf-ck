module Main where

import BrainFuck.Parse (BrainFuckAST (..), ParseError (..), parse)
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
      squashingLogic,
      loopTests,
      sanitizationTests
    ]

basicInstructions :: TestTree
basicInstructions =
  testGroup
    "Basic Instructions"
    [ testCase "Parse single DataIncrement" $
        parse "+" @?= Right [DataIncrement 1],
      testCase "Parse single DataDecrement" $
        parse "-" @?= Right [DataDecrement 1],
      testCase "Parse single PtrIncrement" $
        parse ">" @?= Right [PtrIncrement 1],
      testCase "Parse single PtrDecrement" $
        parse "<" @?= Right [PtrDecrement 1],
      testCase "Parse IO tokens" $
        parse ".," @?= Right [PutChar, GetChar]
    ]

squashingLogic :: TestTree
squashingLogic =
  testGroup
    "Squashing Logic"
    [ testCase "Collapse multiple pluses" $
        parse "+++" @?= Right [DataIncrement 3],
      testCase "Collapse multiple minuses" $
        parse "----" @?= Right [DataDecrement 4],
      testCase "Collapse pointer moves" $
        parse ">>>" @?= Right [PtrIncrement 3],
      testCase "Do not collapse different tokens" $
        parse "+>" @?= Right [DataIncrement 1, PtrIncrement 1],
      testCase "Collapse inside loop" $
        parse "[+++]" @?= Right [Loop [DataIncrement 3]],
      testCase "Collapse inside nested loop" $
        parse "[[+++]]" @?= Right [Loop [Loop [DataIncrement 3]]]
    ]

loopTests :: TestTree
loopTests =
  testGroup
    "Loop and Nesting"
    [ testCase "Empty loop" $
        parse "[]" @?= Right [Loop []],
      testCase "Simple loop body" $
        parse "[+]" @?= Right [Loop [DataIncrement 1]],
      testCase "Nested loops" $
        parse "[>[-] <]"
          @?= Right
            [ Loop
                [ PtrIncrement 1,
                  Loop [DataDecrement 1],
                  PtrDecrement 1
                ]
            ],
      testCase "Multiple loops in sequence" $
        parse "[+][-]" @?= Right [Loop [DataIncrement 1], Loop [DataDecrement 1]],
      testCase "Loop not closed" $
        parse "[" @?= Left UnmatchedLoopOpen,
      testCase "Loop not opened" $
        parse "]" @?= Left UnmatchedLoopClose,
      testCase "More opens than closes" $
        parse "[[>>>[]]" @?= Left UnmatchedLoopOpen,
      testCase "More closes than opens" $
        parse "[>[>>]]]" @?= Left UnmatchedLoopClose
    ]

sanitizationTests :: TestTree
sanitizationTests =
  testGroup
    "Sanitization (Comments/Whitespace)"
    [ testCase "Ignore letters and spaces" $
        parse "+ + hello >" @?= Right [DataIncrement 2, PtrIncrement 1],
      testCase "Ignore newlines" $
        parse "+\n+" @?= Right [DataIncrement 2]
    ]
