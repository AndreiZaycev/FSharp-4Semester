module Workflow

type RoundingBuilder(accuracy : int) =
    member this.Bind (x : float, f) = (x, accuracy) |> System.Math.Round |> f
    member this.Return (x : float) = System.Math.Round (x, accuracy)
    
/// Workflow builder for operations with string integer numbers.
type CalculateBuilder() =
    member this.Bind (x : string, f) =
        match System.Int32.TryParse x with
        | true, number -> number |> f
        | _ -> None 
    member this.Return (x : int) = Some x