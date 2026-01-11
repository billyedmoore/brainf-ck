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
      dataArithmeticToSetCellLogic,
      pointlessLoopLogic,
      clobberAfterWrite
    ]

squashingLogic :: TestTree
squashingLogic =
  testGroup
    "Squashing Logic"
    [ testCase "Collapse multiple pluses" $
        parseAndOptimise "+++" @?= Right [SetCell 3],
      testCase "Collapse multiple minuses" $
        parseAndOptimise "----" @?= Right [SetCell 252],
      testCase "Collapse pointer moves" $
        parseAndOptimise ">>>" @?= Right [PtrArithmetic 3],
      testCase "Do not collapse different tokens" $
        parseAndOptimise "+>" @?= Right [SetCell 1, PtrArithmetic 1],
      testCase "Collapse inside loop" $
        parseAndOptimise "+[+++.]" @?= Right [SetCell 1, Loop [DataArithmetic 3, PutChar]],
      testCase "Collapse inside nested loop" $
        parseAndOptimise "+[[+++.]]" @?= Right [SetCell 1, Loop [Loop [DataArithmetic 3, PutChar]]],
      testCase "Collapse + and - together" $
        parseAndOptimise "++-" @?= Right [SetCell 1],
      testCase "Collapse + and - together more -" $
        parseAndOptimise "----++" @?= Right [SetCell 254],
      testCase "Collapse > and < together" $
        parseAndOptimise ">>>>><<" @?= Right [PtrArithmetic 3],
      testCase "Remove data arithmetic with no net effect" $
        parseAndOptimise "++--" @?= Right [],
      testCase "Remove ptr arithmetic with no net effect" $
        parseAndOptimise "><><" @?= Right []
    ]

dataArithmeticToSetCellLogic :: TestTree
dataArithmeticToSetCellLogic =
  testGroup
    "Data Arithmetic To Set Cell Logic"
    [ testCase "Arithmetic after clear cell" $
        parseAndOptimise ".[-]+++" @?= Right [PutChar, SetCell 3],
      testCase "Arithmetic after clear cell in loop" $
        parseAndOptimise "+++[>[-]++<-]"
          @?= Right [SetCell 3, Loop [PtrArithmetic 1, SetCell 2, PtrArithmetic (-1), DataArithmetic (-1)]],
      testCase "Arithmetic after standard loop" $
        parseAndOptimise "++[>--<+]+++"
          @?= Right
            [ SetCell 2,
              Loop [PtrArithmetic 1, DataArithmetic (-2), PtrArithmetic (-1), DataArithmetic 1],
              SetCell 3
            ],
      testCase
        "Arithmetic after clear inside a loop"
        $
        -- Inside, [-]+++ becomes SetCell 3.
        parseAndOptimise "+[[-]+++]"
          @?= Right [SetCell 1, Loop [SetCell 3]],
      testCase "Multiple resets in a row" $
        -- The second reset [-] makes the first +++++ redundant.
        parseAndOptimise "+[-]+++++[-]++"
          @?= Right [SetCell 2],
      testCase "Multiple resets in a row inside loop" $
        parseAndOptimise "+[[-]+++[-]--]"
          @?= Right
            [SetCell 1, Loop [SetCell 254]],
      testCase
        "SetCell with negative net arithmetic"
        $
        -- [-] followed by 5 '-' results in -5 (or 251 in 8-bit).
        parseAndOptimise "+[-]-----"
          @?= Right [SetCell 251],
      testCase "SetCell after pointer movement" $
        -- Checks that the optimization applies correctly to the new cell.
        parseAndOptimise "+>[-]++++"
          @?= Right [SetCell 1, PtrArithmetic 1, SetCell 4],
      testCase "Arithmetic cancels out after clear" $
        -- Since there is no net effect should all be removed
        parseAndOptimise "+[-]+++---"
          @?= Right []
    ]

clearCellLogic :: TestTree
clearCellLogic =
  testGroup
    "Clear Cell Logic"
    [ testCase "Basic clear cell logic" $
        parseAndOptimise "+[-]" @?= Right [],
      testCase "ClearCell with squashing (net zero/positive)" $
        parseAndOptimise "-[++-]" @?= Right [],
      testCase "ClearCell in nested loop" $
        parseAndOptimise "+[++[-]]" @?= Right [SetCell 1, Loop [SetCell 0]],
      testCase "Clear then modify (should become SetCell 3)" $
        parseAndOptimise "+[-]+++" @?= Right [SetCell 3],
      testCase "Non-optimizable loop (contains pointer moves)" $
        parseAndOptimise "-[>+<]"
          @?= Right [SetCell 255, Loop [PtrArithmetic 1, DataArithmetic 1, PtrArithmetic (-1)]]
    ]

pointlessLoopLogic :: TestTree
pointlessLoopLogic =
  testGroup
    "Pointless Loop Logic"
    [ testCase "Remove opening clear loop" $
        parseAndOptimise "[-]++" @?= Right [SetCell 2],
      testCase "Remove opening loop" $
        parseAndOptimise "[+>]+++" @?= Right [SetCell 3],
      testCase "Remove back to back loop" $
        parseAndOptimise "+[>][<]" @?= Right [SetCell 1, Loop [PtrArithmetic 1]],
      testCase "Remove back to back loops in loop" $
        parseAndOptimise "+[++[+<][>]]"
          @?= Right [SetCell 1, Loop [DataArithmetic 2, Loop [DataArithmetic 1, PtrArithmetic (-1)]]]
    ]

clobberAfterWrite :: TestTree
clobberAfterWrite =
  testGroup
    "Clobber after write"
    [ testCase "DataArithmetic before clear cell removed" $
        parseAndOptimise "+++[-]" @?= Right [],
      testCase "DataArithmetic before PutChar not removed" $
        parseAndOptimise "+++++." @?= Right [SetCell 5, PutChar],
      testCase "DataArithmetic before GetChar removed" $
        parseAndOptimise "+++++," @?= Right [GetChar],
      testCase "DataArithmetic before Loop not removed" $
        parseAndOptimise "+++++---[>]" @?= Right [SetCell 2, Loop [PtrArithmetic 1]]
    ]
