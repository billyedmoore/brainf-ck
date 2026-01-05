module TestParse (parseTests) where

import BrainFuck (BrainFuckAST (..), ParseError (..), parse)
import Test.Tasty
import Test.Tasty.HUnit

parseTests :: TestTree
parseTests =
  testGroup
    "Brainfuck.Parse Unit Tests"
    [ basicInstructions,
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
