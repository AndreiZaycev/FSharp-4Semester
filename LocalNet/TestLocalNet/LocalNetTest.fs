module TestLocalNet

open System.Collections.Generic
open NUnit.Framework
open FsUnit
open LocalNet

[<Test>]
let testWithProbabilities1 () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let foo os =
        match os with
        | Linux -> 1.
        | Windows -> 1.
        | MacOS -> 1.
    let virus = Virus([|computer1|], foo, System.Random().NextDouble)
    let virus1 = Virus([||], foo, System.Random().NextDouble)
    let virus2 = Virus([||], foo, System.Random().NextDouble)
    let network = PlagueInc([| computer1; computer2; |],  Array2D.init 2 2 (fun _ _ -> true), [| virus; virus1; virus2; |])
    let turns = network.Play()
    turns |> should equal 2
      
[<Test>]
let oneMoreTestWithProbabilities1 () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let computer3 = Computer("Computer2", MacOS)
    let foo os =
        match os with
        | Linux -> 1.
        | Windows -> 1.
        | MacOS -> 1.
    let virus = Virus([|computer1|], foo, System.Random().NextDouble)
    let virus1 = Virus([||], foo, System.Random().NextDouble)
    let virus2 = Virus([||], foo, System.Random().NextDouble)
    let arr = [| [| true; true; false |]; [| true; true; true |]; [| false; true; true |] |]
    let network = PlagueInc([|computer1; computer2; computer3|],  Array2D.init 3 3 (fun i j -> arr[i][j]), [| virus; virus1; virus2; |])
    let turns = network.Play()
    turns |> should equal 3
    
[<Test>]
let testThatVirusInfectedAllComputers () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let computer3 = Computer("Computer3", MacOS)
    let foo os =
        match os with
        | Linux -> 1.
        | Windows -> 1.
        | MacOS -> 1.
    let virus = Virus([|computer1|], foo, System.Random().NextDouble)
    let virus1 = Virus([|computer2|], foo, System.Random().NextDouble)
    let arr = [| [| true; true; false |]; [| true; true; true |]; [| false; true; true |] |]
    let network = PlagueInc([|computer1; computer2; computer3|],  Array2D.init 3 3 (fun i j -> arr[i][j]), [| virus; virus1 |])
    let _ = network.Play()
    virus.InfectedComputers.Count |> should equal 3
    HashSet([|computer1; computer2; computer3|]) |> should equal virus.InfectedComputers
    virus1.InfectedComputers.Count |> should equal 3
    HashSet([|computer1; computer2; computer3|]) |> should equal virus.InfectedComputers
    
        
    
[<Test>]                     
let determinedRandomTest () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let computer3 = Computer("Computer2", MacOS)
    let foo1 = fun () -> 0.15
    let foo os =
        match os with
        | Linux -> 0.5
        | Windows -> 0.5
        | MacOS -> 0.5
    let virus = Virus([|computer1|], foo, foo1)
    let virus1 = Virus([||], foo, foo1)
    let virus2 = Virus([||], foo, foo1)
    let arr = [| [| true; true; false |]; [| true; true; true |]; [| false; true; true |] |]
    let network = PlagueInc([|computer1; computer2; computer3|],  Array2D.init 3 3 (fun i j -> arr[i][j]), [| virus; virus1; virus2; |])
    let turns = network.Play()
    turns |> should equal 3

    
[<Test>]                     
let stableTest () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let computer3 = Computer("Computer2", MacOS)
    let foo os =
        match os with
        | Linux -> 0.5
        | Windows -> 0.5
        | MacOS -> 0.5
    let virus = Virus([|computer1|], foo, System.Random().NextDouble)
    let virus1 = Virus([||], foo, System.Random().NextDouble)
    let virus2 = Virus([||], foo, System.Random().NextDouble)
    let network = PlagueInc([|computer1; computer2; computer3|],  Array2D.init 3 3 (fun _ _ -> false), [| virus1; virus2; virus |])
    let turns = network.Play()
    turns |> should equal 1
    
[<Test>]                     
let oneMoreStableTest () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let computer3 = Computer("Computer2", MacOS)
    let foo os =
        match os with
        | Linux -> 0.
        | Windows -> 0.
        | MacOS -> 0.
    let virus = Virus([|computer1|], foo, System.Random().NextDouble)
    let virus1 = Virus([||], foo, System.Random().NextDouble)
    let virus2 = Virus([||], foo, System.Random().NextDouble)
    let network = PlagueInc([|computer1; computer2; computer3|],  Array2D.init 3 3 (fun _ _ -> false), [| virus1; virus2; virus |])
    let turns = network.Play()
    turns |> should equal 1  