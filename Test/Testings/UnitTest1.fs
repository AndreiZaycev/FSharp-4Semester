module Testings

open FsCheck
open FsUnit
open Program
open NUnit.Framework

[<Test>]
let TestMinimum() =
    let firstList = [ for i in 1 .. 100 -> 1 / i ]
    let secondList = [ 3; 2; 1; 0; -1; 12; 14]
    let thirdList = [ 0; 0; 0; 0; 0 ]
    let fourthList = []
    minimum firstList |> should equal (Some (1 / 100))
    minimum secondList |> should equal (Some -1)
    minimum thirdList |> should equal (Some 0)
    minimum fourthList |> should equal Option.None
    
[<Test>]
let TestBinaryTreeTraverse() =
    let tree = BinaryTree.Node (2.,
                            BinaryTree.Node (3., BinaryTree.Node (4., BinaryTree.Leaf 6., BinaryTree.Leaf 3.),
                                             BinaryTree.Leaf 2),
                            None)
    traverse (fun elem -> elem % 2. = 0.) tree |> should equal [2.; 4.; 6.; 2.]
    traverse (fun elem -> elem % 2. = 1.) tree |> should equal [3.; 3.]
    traverse (fun _ -> true) tree |> should equal [2.; 3.; 4.; 6.; 3.; 2.]
    
[<Test>]
let HashMapTest() =
    let hs = HashTable((fun elem -> elem.ToString().Length))
    hs.Add 1 2
    hs.Add 21 3
    hs.Remove 21
    (hs.Get 1) |> should equal <| Option.Some 2
    (hs.Get 21) |> should equal Option.None