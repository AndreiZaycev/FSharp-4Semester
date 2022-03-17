module InterpreterTests

open NUnit.Framework
open FsUnit
open LambdaInterpreter

[<Test>]
let exampleFromAbstract () =
    let value =
        reduce (
            Application(
                Abstraction("x", Variable "y"),
                Application(
                    Abstraction("x", Application(Variable "x", Application(Variable "x", Variable "x"))),
                    Abstraction("x", Application(Variable "x", Application(Variable "x", Variable "x")))
                )
            )
        )

    value |> should equal (Variable "y")

[<Test>]
let secondTest () =
    let value =
        reduce (Abstraction("x", Application(Abstraction("y", Variable("y")), Variable("x"))))

    value |> should equal (Abstraction("x", Variable("x")))