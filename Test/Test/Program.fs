/// Gets type of generic item
let inline tryGetMaxValue item =
    match item.GetType().GetField("MaxValue") with
    | null -> None
    | fieldInfo -> fieldInfo.GetValue() |> unbox< ^a> |> Some
  
/// finds minimum in the list   
let inline minimum list =
    match list with
    | [] -> None
    | _ ->
        let maxValue = (tryGetMaxValue list.Head).Value
        Some(List.fold(fun acc elem -> min elem acc) maxValue list)
  
/// simple binary tree implementation  
type BinaryTree<'t> =
    | None
    | Leaf of 't 
    | Node of 't * BinaryTree<'t> * BinaryTree<'t>
  
/// binary tree traverse with some expression checker  
let traverse expression tree =
    let rec _helper acc tree =
        match tree with
        | None -> acc 
        | Leaf t ->
            if expression t
            then t :: acc
            else acc
        | Node(t, left, right) ->
            let subtrees = _helper (_helper acc right) left
            if expression t
            then t :: subtrees
            else subtrees
    _helper [] tree
    
/// simple hash table implementation 
type HashTable(hashFunction: 't -> int) =
     let mutable hash = hashFunction
     
     let maxSize = int(2. ** 13)
     
     let mutable _items = [| for i in 0 .. maxSize - 1 -> Option.None |] 
     
     member this.Add key value =
         let hashedKey = hash key
         if hashedKey > maxSize
         then failwith "Hashed key is out of range"
         else 
            _items[hashedKey] <- Some(value)
            
     member this.Remove key =
         let hashedKey = hash key
         if hashedKey > maxSize
         then failwith "Hashed key is out of range"
         else
             _items[hashedKey] <- Option.None 
             
     member this.Get key =
         let hashedKey = hash key
         if hashedKey > maxSize
         then failwith "Hashed key is out of range"
         else
             _items[hashedKey]
           
            