module TestLocalNet

open NUnit.Framework
open FsUnit
open LocalNet
open Foq


[<Test>]
let testWithProbabilities1 () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let virus1 = Virus("ivanMOSKALENKO", System.Random().NextDouble, 1, 1, 1, List.singleton computer1)
    let virus2 = Virus("ALEXANDERKURDAKOV", System.Random().NextDouble, 1, 1, 1, List.empty)
    let virus3 = Virus("DEDSEC", System.Random().NextDouble, 1, 1, 1, List.empty)
    let network = Net([ computer1; computer2; ],  Array2D.init 2 2 (fun _ _ -> true), [ virus1; virus2; virus3 ])
    let turns = network.Start()
    turns |> should equal 2
      
[<Test>]
let oneMoreTestWithProbabilities1 () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let computer3 = Computer("Computer2", MacOS)
    let virus1 = Virus("ivanMOSKALENKO", System.Random().NextDouble, 1, 1, 1, List.singleton computer1)
    let virus2 = Virus("ALEXANDERKURDAKOV", System.Random().NextDouble, 1, 1, 1, List.empty)
    let virus3 = Virus("DEDSEC", System.Random().NextDouble, 1, 1, 1, List.empty)
    let arr = [| [| true; true; false |]; [| true; true; true |]; [| false; true; true |] |]
    let network = Net([computer1; computer2; computer3],  Array2D.init 3 3 (fun i j -> arr[i][j]), [ virus1; virus2; virus3 ])
    let turns = network.Start()
    turns |> should equal 3
    
[<Test>]                     
let determinedRandomTest () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let computer3 = Computer("Computer2", MacOS)
    let foo = fun () -> 0.15
    let virus1 = Virus("ivanMOSKALENKO", foo, 0.5, 0.5, 0.5, List.singleton computer1)
    let virus2 = Virus("ALEXANDERKURDAKOV",foo, 0.5, 0.5, 0.5, List.empty)
    let virus3 = Virus("DEDSEC", foo, 0.5, 0.5, 0.5, List.empty)
    let arr = [| [| true; true; false |]; [| true; true; true |]; [| false; true; true |] |]
    let network = Net([computer1; computer2; computer3],  Array2D.init 3 3 (fun i j -> arr[i][j]), [ virus1; virus2; virus3 ])
    let turns = network.Start()
    turns |> should equal 3

    
[<Test>]                     
let stableTest () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let computer3 = Computer("Computer2", MacOS)
    let virus1 = Virus("ivanMOSKALENKO", System.Random().NextDouble, 0.5, 0.5, 0.5, List.singleton computer1)
    let virus2 = Virus("ALEXANDERKURDAKOV",System.Random().NextDouble, 0.5, 0.5, 0.5, List.empty)
    let virus3 = Virus("DEDSEC", System.Random().NextDouble, 0.5, 0.5, 0.5, List.empty)
    let network = Net([computer1; computer2; computer3],  Array2D.init 3 3 (fun _ _ -> false), [ virus1; virus2; virus3 ])
    let turns = network.Start()
    turns |> should equal 1
    
[<Test>]                     
let oneMoreStableTest () =
    let computer1 = Computer("Computer1", Linux)
    let computer2 = Computer("Computer2", Windows)
    let computer3 = Computer("Computer2", MacOS)
    let virus1 = Virus("ivanMOSKALENKO", System.Random().NextDouble, 0, 0, 0, List.singleton computer1)
    let virus2 = Virus("ALEXANDERKURDAKOV",System.Random().NextDouble, 0, 0, 0, List.empty)
    let virus3 = Virus("DEDSEC", System.Random().NextDouble, 0, 0, 0, List.empty)
    let network = Net([computer1; computer2; computer3],  Array2D.init 3 3 (fun _ _ -> false), [ virus1; virus2; virus3 ])
    let turns = network.Start()
    turns |> should equal 1  