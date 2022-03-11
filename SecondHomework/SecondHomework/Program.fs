module functions

let mapEven list =
    (List.map (fun a -> (a + 1) % 2) list)
    |> List.reduce (+)

let foldEven (list: List<int>) =
    List.fold (fun acc a -> acc + (a + 1) % 2) 0 list

let filterEven list =
    (List.filter (fun a -> a % 2 = 0) list)
    |> List.length

type BinaryTree<'t> =
    | None
    | Node of 't * BinaryTree<'t> * BinaryTree<'t>

let mapBinaryTree func tree =
    let rec helper tree =
        match tree with
        | None -> None
        | Node (a, left, right) -> Node(func a, helper left, helper right)

    helper tree

type Operations =
    | Sum
    | Multiply
    | Subtract
    | Divide

type ArithmeticTree =
    | Leaf of int
    | Node of Operations * ArithmeticTree * ArithmeticTree

let arithmeticTree1 =
    Node(Sum, Leaf 5, Node(Multiply, Leaf 3, Leaf 2))

let rec evaluateExpression tree =
    match tree with
    | Node (operation, left, right) ->
        match operation with
        | Sum -> (+) (evaluateExpression left) (evaluateExpression right)
        | Subtract -> (-) (evaluateExpression left) (evaluateExpression right)
        | Divide -> (/) (evaluateExpression left) (evaluateExpression right)
        | Multiply -> (*) (evaluateExpression left) (evaluateExpression right)
    | Leaf a -> a

let primeGenerator =
    let isPrime n =
        let rec check i =
            i > n / 2 || (n % i <> 0 && check (i + 1))

        check 2

    let mutable counter = 2

    seq {
        yield 1

        while true do
            if isPrime counter then
                printfn "%A" counter
                yield counter

            counter <- counter + 1
    }