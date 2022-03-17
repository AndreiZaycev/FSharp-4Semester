module LambdaInterpreter


/// Expression type 
type Expression = 
    | Variable of string
    | Application of Expression * Expression
    | Abstraction of string * Expression

/// Apply beta reduction 
let rec reduce exp =   

    /// Checks if a variable is free in the expression
    let rec isFree expression x =
        match expression with
        | Variable a -> a = x
        | Application(left, right) -> isFree left x || isFree right x
        | Abstraction(a, exp) -> a <> x && isFree exp x
    
    /// Substitute term 
    let rec substitution variable term expression =
        match expression with
        | Variable x when x = variable -> term | Variable _ -> expression
        | Abstraction(x, _) when x = variable -> expression
        | Abstraction(x, e) when not (isFree term x) -> Abstraction(x, substitution variable term e)
        | Abstraction(x, e) ->
            let list = ['a'..'Z'] |> List.filter (fun t -> not (isFree term (string t))) |> List.map string
            let name = 
                if list.IsEmpty
                then
                    let rec search x =
                        if isFree e x
                        then x
                        else search x + "1"
                    search "a"
                else list.Head
            Abstraction(name, substitution variable term (substitution x (Variable name) e))
        | Application(l, r) -> Application(substitution variable term l, substitution variable term r)

    /// Apply reduce
    let rec red exp =
        match exp with
        | Application(l, r) ->
            match red l with
            | Abstraction(x, e) -> substitution x r e
            | x -> Application(x, r)
        | _ -> exp

    match exp with
    | Variable _ -> exp
    | Application(l, r) ->
        match red l with
        | Abstraction(x, e) -> reduce <| substitution x r e
        | x -> Application(reduce x, reduce r)
    | Abstraction(v, e) -> Abstraction(v, reduce e)