module Tests
open NUnit.Framework
open FsUnit
open Program

[<Test>]
let Test() =
    let first = ""
    let second = "()"
    let third = "([{}])"
    let fourth = "(()))"
    let fifth = "((())"
    let sixth = "(){}[]"
    checker first |> should equal true
    checker second |> should equal true
    checker third |> should equal true
    checker fourth |> should equal false
    checker fifth |> should equal false
    checker sixth |> should equal true
