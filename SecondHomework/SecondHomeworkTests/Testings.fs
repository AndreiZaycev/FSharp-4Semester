module SecondHomeworkTests

open NUnit.Framework
open functions

[<Test>]
let EqualityMapFoldFilter () =
    let firstList = [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ]

    let secondList =
        [ 11
          12
          13
          14
          15
          17
          19
          228
          1337
          19 ]

    let thirdList = [ 228; 228; 228; 228 ]

    let checkOnList list =
        Assert.AreEqual(mapEven list, filterEven list)
        Assert.AreEqual(mapEven list, foldEven list)
        Assert.AreEqual(filterEven list, foldEven list)

    checkOnList firstList
    checkOnList secondList
    checkOnList thirdList

[<Test>]
let MapTreeTest () =
    let tree =
        BinaryTree<float>.Node (2., BinaryTree<float>.Node (3., BinaryTree<float>.Node (4., None, None), None), None)

    let treeAfterApplyingIterFunction =
        BinaryTree<float>.Node (3., BinaryTree<float>.Node (4., BinaryTree<float>.Node (5., None, None), None), None)

    let treeAfterApplyingPow2 =
        BinaryTree<float>.Node (4., BinaryTree<float>.Node (9., BinaryTree<float>.Node (16., None, None), None), None)

    Assert.AreEqual(mapBinaryTree (fun a -> a ** 2.) tree, treeAfterApplyingPow2)
    Assert.AreEqual(mapBinaryTree (fun a -> a + 1.) tree, treeAfterApplyingIterFunction)


[<Test>]
let arithmeticTreeTest () =
    let firstTree = Node(Sum, Leaf 114, Leaf 114)

    let secondTree =
        Node(Multiply, Node(Sum, Leaf 2, Leaf 2), Node(Divide, Leaf 4, Leaf 2))

    Assert.AreEqual(228, evaluateExpression firstTree)
    Assert.AreEqual(8, evaluateExpression secondTree)
