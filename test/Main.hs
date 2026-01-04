module Main where

import BrainFuck (BrainFuckAST (..), ParseError (..), parse, parseAndOptimize)
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
        parse "+" @?= Right [DataArithmetic 1],
      testCase "Parse single DataDecrement" $
        parse "-" @?= Right [DataArithmetic (-1)],
      testCase "Parse single PtrIncrement" $
        parse ">" @?= Right [PtrArithmetic 1],
      testCase "Parse single PtrDecrement" $
        parse "<" @?= Right [PtrArithmetic (-1)],
      testCase "Parse IO tokens" $
        parse ".," @?= Right [PutChar, GetChar]
    ]

squashingLogic :: TestTree
squashingLogic =
  testGroup
    "Squashing Logic"
    [ testCase "Collapse multiple pluses" $
        parseAndOptimize "+++" @?= Right [DataArithmetic 3],
      testCase "Collapse multiple minuses" $
        parseAndOptimize "----" @?= Right [DataArithmetic (-4)],
      testCase "Collapse pointer moves" $
        parseAndOptimize ">>>" @?= Right [PtrArithmetic 3],
      testCase "Do not collapse different tokens" $
        parseAndOptimize "+>" @?= Right [DataArithmetic 1, PtrArithmetic 1],
      testCase "Collapse inside loop" $
        parseAndOptimize "[+++.]" @?= Right [Loop [DataArithmetic 3, PutChar]],
      testCase "Collapse inside nested loop" $
        parseAndOptimize "[[+++.]]" @?= Right [Loop [Loop [DataArithmetic 3, PutChar]]],
      testCase "Collapse + and - together" $
        parseAndOptimize "++-" @?= Right [DataArithmetic 1],
      testCase "Collapse + and - together more -" $
        parseAndOptimize "----++" @?= Right [DataArithmetic (-2)],
      testCase "Collapse > and < together" $
        parseAndOptimize ">>>>><<" @?= Right [PtrArithmetic 3],
      testCase "Remove data arithmetic with no net effect" $
        parseAndOptimize "++--" @?= Right [],
      testCase "Remove ptr arithmetic with no net effect" $
        parseAndOptimize "><><" @?= Right []
    ]

loopTests :: TestTree
loopTests =
  testGroup
    "Loop and Nesting"
    [ testCase "Empty loop" $
        parse "[]" @?= Right [Loop []],
      testCase "Simple loop body" $
        parse "[+]" @?= Right [Loop [DataArithmetic 1]],
      testCase "Nested loops" $
        parse "[>[->] <]"
          @?= Right
            [ Loop
                [ PtrArithmetic 1,
                  Loop [DataArithmetic (-1), PtrArithmetic (1)],
                  PtrArithmetic (-1)
                ]
            ],
      testCase "Multiple loops in sequence" $
        parse "[+][-]" @?= Right [Loop [DataArithmetic 1], Loop [DataArithmetic (-1)]],
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
        parse "+ + hello >" @?= Right [DataArithmetic 1, DataArithmetic 1, PtrArithmetic 1],
      testCase "Ignore newlines" $
        parse "+\n+" @?= Right [DataArithmetic 1, DataArithmetic 1]
    ]
