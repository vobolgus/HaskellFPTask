import Test.HUnit
import Geometry -- Import geometry module

-- Test cases
testPreprocess1 = TestCase (assertEqual "for (preprocessProg (LineSegment 1 1 1 1))," 
                                        (Point 1 1) 
                                        (preprocessProg (LineSegment 1 1 1 1)))

testPreprocess2 = TestCase (assertEqual "for (preprocessProg (LineSegment 2 3 1 1))," 
                                        (LineSegment 1 1 2 3) 
                                        (preprocessProg (LineSegment 2 3 1 1)))

testEval1 = TestCase (assertEqual "for (evalProg (Point 1 2) [])," 
                                (Point 1 2) 
                                (evalProg (Point 1 2) []))

testEval2 = TestCase (assertEqual "for (evalProg (Intersect (Line 1 0) (VerticalLine 1)) [])," 
                                (Point 1 1) 
                                (evalProg (Intersect (Line 1 0) (VerticalLine 1)) []))

testEval3 = TestCase (assertEqual "for (evalProg (Let \"x\" (Point 1 2) (Var \"x\")) [])," 
                                (Point 1 2) 
                                (evalProg (Let "x" (Point 1 2) (Var "x")) []))

testShift1 = TestCase (assertEqual "for (shift 1 1 (Point 1 2))," 
                                  (Point 2 3) 
                                  (shift 1 1 (Point 1 2)))

testShift2 = TestCase (assertEqual "for (shift 1 1 (Line 1 0))," 
                                  (Line 1 0) 
                                  (shift 1 1 (Line 1 0)))

tests = TestList [ TestLabel "testPreprocess1" testPreprocess1
                 , TestLabel "testPreprocess2" testPreprocess2
                 , TestLabel "testEval1" testEval1
                 , TestLabel "testEval2" testEval2
                 , TestLabel "testEval3" testEval3
                 , TestLabel "testShift1" testShift1
                 , TestLabel "testShift2" testShift2
                 ]

main :: IO Counts
main = runTestTT tests
