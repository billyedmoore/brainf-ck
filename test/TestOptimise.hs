module TestOptimise (optimiseTests) where

import BrainFuck (BrainFuckAST (..), parseAndOptimise)
import Test.Tasty
import Test.Tasty.HUnit

optimiseTests :: TestTree
optimiseTests =
  testGroup
    "Brainfuck.Optimise Unit Tests"
    [ squashingLogic,
      clearCellLogic,
      pointlessLoopLogic
    ]

squashingLogic :: TestTree
squashingLogic =
  testGroup
    "Squashing Logic"
    [ testCase "Collapse multiple pluses" $
        parseAndOptimise "+++" @?= Right [DataArithmetic 3],
      testCase "Collapse multiple minuses" $
        parseAndOptimise "----" @?= Right [DataArithmetic (-4)],
      testCase "Collapse pointer moves" $
        parseAndOptimise ">>>" @?= Right [PtrArithmetic 3],
      testCase "Do not collapse different tokens" $
        parseAndOptimise "+>" @?= Right [DataArithmetic 1, PtrArithmetic 1],
      testCase "Collapse inside loop" $
        parseAndOptimise "+[+++.]" @?= Right [DataArithmetic 1, Loop [DataArithmetic 3, PutChar]],
      testCase "Collapse inside nested loop" $
        parseAndOptimise "+[[+++.]]" @?= Right [DataArithmetic 1, Loop [Loop [DataArithmetic 3, PutChar]]],
      testCase "Collapse + and - together" $
        parseAndOptimise "++-" @?= Right [DataArithmetic 1],
      testCase "Collapse + and - together more -" $
        parseAndOptimise "----++" @?= Right [DataArithmetic (-2)],
      testCase "Collapse > and < together" $
        parseAndOptimise ">>>>><<" @?= Right [PtrArithmetic 3],
      testCase "Remove data arithmetic with no net effect" $
        parseAndOptimise "++--" @?= Right [],
      testCase "Remove ptr arithmetic with no net effect" $
        parseAndOptimise "><><" @?= Right []
    ]

clearCellLogic :: TestTree
clearCellLogic =
  testGroup
    "Clear Cell Logic"
    [ testCase "Basic clear cell logic" $
        parseAndOptimise "+[-]" @?= Right [DataArithmetic 1, ClearCell],
      testCase "ClearCell with squashing (net zero/positive)" $
        parseAndOptimise "-[++-]" @?= Right [DataArithmetic (-1), ClearCell],
      testCase "ClearCell in nested loop" $
        parseAndOptimise "+[++[-]]" @?= Right [DataArithmetic 1, Loop [DataArithmetic 2, ClearCell]],
      testCase "Clear then modify (should remain ClearCell + Arith)" $
        parseAndOptimise "+[-]+++" @?= Right [DataArithmetic 1, ClearCell, DataArithmetic 3],
      testCase "Non-optimizable loop (contains pointer moves)" $
        parseAndOptimise "-[>+<]"
          @?= Right [DataArithmetic (-1), Loop [PtrArithmetic 1, DataArithmetic 1, PtrArithmetic (-1)]]
    ]

pointlessLoopLogic :: TestTree
pointlessLoopLogic =
  testGroup
    "Pointless Loop Logic"
    [ testCase "Remove opening clear loop" $
        parseAndOptimise "[-]++" @?= Right [DataArithmetic 2],
      testCase "Remove opening loop" $
        parseAndOptimise "[+>]+++" @?= Right [DataArithmetic 3],
      testCase "Remove back to back loop" $
        parseAndOptimise "+[>][<]" @?= Right [DataArithmetic 1, Loop [PtrArithmetic 1]],
      testCase "Remove back to back loops in loop" $
        parseAndOptimise "+[++[+<][>]]"
          @?= Right [DataArithmetic 1, Loop [DataArithmetic 2, Loop [DataArithmetic 1, PtrArithmetic (-1)]]]
    ]
