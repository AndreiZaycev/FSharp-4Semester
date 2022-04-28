module Workflow

/// rounding builder for operations with given round accuracy
type RoundingBuilder(accuracy : int) =
    member this.Bind (x : float, f) = System.Math.Round(x, accuracy) |> f
    member this.Return (x : float) = System.Math.Round (x, accuracy)
    
/// workflow builder for operations with string integer numbers
type CalculateBuilder() =
    member this.Bind (x : string, f) =
        match System.Int32.TryParse x with
        | true, number -> f number 
        | _ -> None 
    member this.Return (x : int) = Some x