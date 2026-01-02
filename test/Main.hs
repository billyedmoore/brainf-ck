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
        parse "+++" @?= Right [DataArithmetic 3],
      testCase "Collapse multiple minuses" $
        parse "----" @?= Right [DataArithmetic (-4)],
      testCase "Collapse pointer moves" $
        parse ">>>" @?= Right [PtrArithmetic 3],
      testCase "Do not collapse different tokens" $
        parse "+>" @?= Right [DataArithmetic 1, PtrArithmetic 1],
      testCase "Collapse inside loop" $
        parse "[+++]" @?= Right [Loop [DataArithmetic 3]],
      testCase "Collapse inside nested loop" $
        parse "[[+++]]" @?= Right [Loop [Loop [DataArithmetic 3]]],
      testCase "Collapse + and - together" $
        parse "++-" @?= Right [DataArithmetic 1],
      testCase "Collapse + and - together more -" $
        parse "----++" @?= Right [DataArithmetic (-2)],
      testCase "Collapse > and < together" $
        parse ">>>>><<" @?= Right [PtrArithmetic 3],
      testCase "Remove data arithmetic with no net effect" $
        parse "++--" @?= Right [],
      testCase "Remove ptr arithmetic with no net effect" $
        parse "><><" @?= Right []
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
        parse "[>[-] <]"
          @?= Right
            [ Loop
                [ PtrArithmetic 1,
                  Loop [DataArithmetic (-1)],
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
        parse "+ + hello >" @?= Right [DataArithmetic 2, PtrArithmetic 1],
      testCase "Ignore newlines" $
        parse "+\n+" @?= Right [DataArithmetic 2]
    ]
