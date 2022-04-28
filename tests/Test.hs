import Test.Tasty
import Common
import Hw3

main :: IO ()
main = runTests [ unit ]

unit :: Score -> TestTree
unit sc = testGroup "Unit 2" [
  scoreTest (sqSum, [], 0, 5, "sqSum 1"),
  scoreTest (sqSum, [1,2,3,4], 30, 5, "sqSum 2"),
  scoreTest (sqSum, [-1,-2,-3,-4], 30, 5, "sqSum 3"),
  scoreTest (uncurry pipe, ([], 3), 3, 10, "pipe 1"),
  scoreTest (uncurry pipe, ([(\x-> 2*x),(\x -> x + 3)], 3), 12, 10, "pipe 2"),
  scoreTest (uncurry pipe, ([(\x -> x + 3), (\x-> 2*x)], 3), 9, 10, "pipe 3"),

  scoreTest(uncurry sepConcat, (", ",["foo","bar","baz"]), "foo, bar, baz", 5, "sepConcat 1"),
  scoreTest(uncurry sepConcat, ("---",[]), "", 5, "sepConcat 2"),
  scoreTest(uncurry sepConcat, ("",["a","b","c","d","e"]), "abcde", 5, "sepConcat 3"),
  scoreTest(uncurry sepConcat, ("X",["hello"]), "hello", 5, "sepConcat 4"),

  scoreTest(uncurry stringOfList, (intString, [1,2,3,4,5,6]), "[1, 2, 3, 4, 5, 6]",3,"stringOfList 1"),
  scoreTest(uncurry stringOfList, (id, ["foo"]), "[foo]",3,"stringOfList 2"),
  scoreTest(uncurry stringOfList, ((stringOfList intString),[[1,2,3],[4,5],[6],[]]), "[[1, 2, 3], [4, 5], [6], []]",4,"stringOfList 3"),

  scoreTest(uncurry clone, (3,5), [3,3,3,3,3],5,"clone 1"),
  scoreTest(uncurry clone, ("foo",2), ["foo","foo"],5,"clone 2"),

  scoreTest(uncurry padZero, ([9,9],[1,0,0,2]), ([0,0,9,9],[1,0,0,2]),3,"padzero 1"),
  scoreTest(uncurry padZero, ([1,0,0,2],[9,9]), ([1,0,0,2],[0,0,9,9]),2,"padzero 2"),

  scoreTest(removeZero, [0,0,0,1,0,0,2], [1,0,0,2],4,"removeZero 1"),
  scoreTest(removeZero, [9,9], [9,9],3,"removeZero 2"),
  scoreTest(removeZero, [0,0,0], [],3,"removeZero 3"),

  scoreTest(uncurry bigAdd,  ([9,9],[1,0,0,2]), [1,1,0,1],10, "bigAdd 1"),
  scoreTest(uncurry bigAdd,  ([9,9,9,9],[9,9,9]), [1,0,9,9,8],10, "bigAdd 2"),
  scoreTest(uncurry bigAdd,  ([1,0,0],[1]), [1,0,1],5, "bigAdd 3"),

  scoreTest(uncurry mulByInt,  (9,[9,9,9,9]), [8,9,9,9,1],15, "mulByInt 1"),

  scoreTest(uncurry bigMul,  ([9,9,9,9],[9,9,9,9]), [9,9,9,8,0,0,0,1],5, "bigMul 1"),
  scoreTest(uncurry bigMul,  ([9,9,9,9,9],[9,9,9,9,9]), [9,9,9,9,8,0,0,0,0,1],5,"bigMul 2"),
  scoreTest(uncurry bigMul,  ([8,9],[6,7,0]), [5,9,6,3,0],5,"bigMul 3"),
  scoreTest(uncurry bigMul,  ([6,7,0],[8,9]), [5,9,6,3,0],5,"bigMul 4")
  ]
  where
    scoreTest :: (Show b, Eq b) => (a -> b, a, b, Int, String) -> TestTree
    scoreTest (f, x, r, n, msg) = scoreTest' sc (return . f, x, r, n, msg)
