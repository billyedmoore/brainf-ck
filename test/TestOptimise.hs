module TestOptimise (optimiseTests) where

import BrainFuck (BrainFuckAST (..), parseAndOptimize)
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
        parseAndOptimize "+++" @?= Right [DataArithmetic 3],
      testCase "Collapse multiple minuses" $
        parseAndOptimize "----" @?= Right [DataArithmetic (-4)],
      testCase "Collapse pointer moves" $
        parseAndOptimize ">>>" @?= Right [PtrArithmetic 3],
      testCase "Do not collapse different tokens" $
        parseAndOptimize "+>" @?= Right [DataArithmetic 1, PtrArithmetic 1],
      testCase "Collapse inside loop" $
        parseAndOptimize "+[+++.]" @?= Right [DataArithmetic 1, Loop [DataArithmetic 3, PutChar]],
      testCase "Collapse inside nested loop" $
        parseAndOptimize "+[[+++.]]" @?= Right [DataArithmetic 1, Loop [Loop [DataArithmetic 3, PutChar]]],
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

clearCellLogic :: TestTree
clearCellLogic =
  testGroup
    "Clear Cell Logic"
    [ testCase "Basic clear cell logic" $
        parseAndOptimize "+[-]" @?= Right [DataArithmetic 1, ClearCell],
      testCase "ClearCell with squashing (net zero/positive)" $
        parseAndOptimize "-[++-]" @?= Right [DataArithmetic (-1), ClearCell],
      testCase "ClearCell in nested loop" $
        parseAndOptimize "+[++[-]]" @?= Right [DataArithmetic 1, Loop [DataArithmetic 2, ClearCell]],
      testCase "Clear then modify (should remain ClearCell + Arith)" $
        parseAndOptimize "+[-]+++" @?= Right [DataArithmetic 1, ClearCell, DataArithmetic 3],
      testCase "Non-optimizable loop (contains pointer moves)" $
        parseAndOptimize "-[>+<]"
          @?= Right [DataArithmetic (-1), Loop [PtrArithmetic 1, DataArithmetic 1, PtrArithmetic (-1)]]
    ]

pointlessLoopLogic :: TestTree
pointlessLoopLogic =
  testGroup
    "Pointless Loop Logic"
    [ testCase "Remove opening clear loop" $
        parseAndOptimize "[-]++" @?= Right [DataArithmetic 2],
      testCase "Remove opening loop" $
        parseAndOptimize "[+>]+++" @?= Right [DataArithmetic 3],
      testCase "Remove back to back loop" $
        parseAndOptimize "+[>][<]" @?= Right [DataArithmetic 1, Loop [PtrArithmetic 1]],
      testCase "Remove back to back loops in loop" $
        parseAndOptimize "+[++[+<][>]]"
          @?= Right [DataArithmetic 1, Loop [DataArithmetic 2, Loop [DataArithmetic 1, PtrArithmetic (-1)]]]
    ]
