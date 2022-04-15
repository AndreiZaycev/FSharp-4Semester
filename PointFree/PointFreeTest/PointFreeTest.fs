module PointFreeTest

open NUnit.Framework
open FsCheck
open Program

[<Test>]
let Test() =
    Check.QuickThrowOnFailure (fun a b -> first a b = third() a b)
    Check.QuickThrowOnFailure (fun a b -> first a b = second a b)
    Check.QuickThrowOnFailure (fun a b -> second a b = third() a b)
    