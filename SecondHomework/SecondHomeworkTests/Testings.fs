module SecondHomeworkTests

open NUnit.Framework
open functions
open FsCheck

[<Test>]
let EqualityMapFoldFilter () =
    let checkOnList list =
        mapEven list = filterEven list && mapEven list = foldEven list && filterEven list = foldEven list
    
    Check.QuickThrowOnFailure(checkOnList)

[<Test>]
let MapTreeTest () =
    let tree =
        BinaryTree.Node (2., BinaryTree.Node (3., BinaryTree.Node (4., None, None), None), None)

    let treeAfterApplyingIterFunction =
        BinaryTree.Node (3., BinaryTree.Node (4., BinaryTree.Node (5., None, None), None), None)

    let treeAfterApplyingPow2 =
        BinaryTree.Node (4., BinaryTree.Node (9., BinaryTree.Node (16., None, None), None), None)
        
    let nullLeafs = BinaryTree.Node(1337, BinaryTree.None, BinaryTree.None)
    
    let nullLeafsAfterApplyingIteration = BinaryTree.Node(228, BinaryTree.None, BinaryTree.None)

    Assert.AreEqual(mapBinaryTree (fun a -> a ** 2.) tree, treeAfterApplyingPow2)
    Assert.AreEqual(mapBinaryTree (fun a -> a + 1.) tree, treeAfterApplyingIterFunction)
    Assert.AreEqual(mapBinaryTree (fun a -> a + 1.) tree, treeAfterApplyingIterFunction)
    Assert.AreEqual(mapBinaryTree (fun a -> a * 228 / 1337) nullLeafs, nullLeafsAfterApplyingIteration)


[<Test>]
let arithmeticTreeTest () =
    let firstTree = Node(Sum, Leaf 114, Leaf 114)

    let secondTree =
        Node(Multiply, Node(Subtract, Leaf 2, Leaf 2), Node(Divide, Leaf 4, Leaf 2))

    Assert.AreEqual(228, evaluateExpression firstTree)
    Assert.AreEqual(0, evaluateExpression secondTree)
