open System.IO
/// checks correctness of bracket sequence 
let checker string =
    let left = ['('; '['; '{'] 
    let right = [')'; ']'; '}']
    let list, isCompleted = List.fold(fun (acc, isGood) elem ->
        match elem with
        | t when List.contains t left -> elem :: acc, isGood
        | t when List.contains t right ->
            match acc with
            | [] -> [], false 
            | hd :: tl -> tl, isGood &&
                              (List.findIndex (fun a -> a = hd) left = List.findIndex (fun a -> a = t) right)
        | _ -> raise(InvalidDataException "String must contains only brackets")) ([], true) (List.ofSeq string)
    list.IsEmpty && isCompleted
    