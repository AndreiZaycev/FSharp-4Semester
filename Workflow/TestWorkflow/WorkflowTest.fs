module TestWorkflow

open System
open NUnit.Framework
open Workflow
open FsUnit

[<Test>]
let testRound3 () =
    let rounding = RoundingBuilder(3)

    let res =
        rounding {
            let! a = 2.0 / 12.0
            let! b = 3.5
            return a / b
        }

    res |> should equal 0.048
    
[<Test>]
let testRoundNegative() =
    let rounding = RoundingBuilder(-1)

    (fun () ->
        rounding {
            let! a = 1.5
            return a;
        } |> ignore) |> should throw typeof<ArgumentOutOfRangeException>

[<Test>]
let testForCalculate3 () =
    let calculate = CalculateBuilder()

    let res =
        calculate {
            let! x = "1"
            let! y = "2"
            let z = x + y
            return z
        }

    res |> should equal (Some 3)

[<Test>]
let incorrectInputTest () =
    let calculate = CalculateBuilder()

    let res =
        calculate {
            let! x = "1"
            let! y = "Ъ"
            let z = x + y 
            return z
        }

    res |> should equal None
    