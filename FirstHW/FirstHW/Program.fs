open System

let factorial =
    let rec helper acc =
        function
        | x when x < 0 -> raise (ArgumentOutOfRangeException("Fibonacci number cannot be negative"))
        | 0 -> acc
        | n -> helper (acc * n) (n - 1)

    helper 1

let fibonacci n =
    if n < 0 then
        raise (ArgumentOutOfRangeException("Fibonacci number cannot be negative"))

    let rec helper n acc acc1 =
        match n with
        | 0 -> acc1
        | 1 -> acc
        | _ -> helper (n - 1) (acc + acc1) acc

    helper n 1 0


let reverse l =
    let rec helper acc =
        function
        | [] -> acc
        | hd :: tail -> helper (hd :: acc) tail

    helper [] l

let specialListNaive n m = [ for i in n .. n + m -> 2. ** i ]

let specialListNotNaive (n: int) m =
    let rec helper acc =
        function
        | k when k = n + m -> acc
        | k when k < n + m -> helper ((acc.Head * 2.) :: acc) (k + 1)
        | _ -> raise (ArgumentOutOfRangeException("Fibonacci number cannot be negative"))

    reverse (helper [ 2. ** n ] n)

let firstOccurence value =
    let rec helper cnt =
        function
        | [] -> -1
        | hd :: tl ->
            if hd = value then
                cnt
            else
                helper (cnt + 1) tl

    helper 0