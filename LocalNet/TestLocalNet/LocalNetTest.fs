module TestLocalNet

open NUnit.Framework
open FsUnit
open LocalNet
open Foq


[<Test>]
let testWithProbabilities1 () =
    let probabilities = Map [ ("Linux", 1.); ("Windows", 1.) ]
    let computer1 = Computer("Computer1", "Linux", probabilities)
    computer1.Infect
    computer1.EndTurn
    let computer2 = Computer("Computer2", "Windows", probabilities)
    let network = Net([ computer1; computer2; ],  Array2D.init 2 2 (fun _ _ -> true), System.Random())
    let turns = network.Start
    turns |> should equal 2
        
[<Test>]
let oneMoreTestWithProbabilities1 () =
    let probabilities = Map [ ("Linux", 1.); ("Windows", 1.) ]
    let computer1 = Computer("Computer1", "Linux", probabilities)
    computer1.Infect
    computer1.EndTurn
    let computer2 = Computer("Computer2", "Windows", probabilities)
    let computer3 = Computer("Computer3", "Windows", probabilities)
    let arr = [| [| true; true; false |]; [| true; true; true |]; [| false; true; true |] |]
    let network = Net([computer1; computer2; computer3],  Array2D.init 3 3 (fun i j -> arr[i][j]), System.Random())
    let turns = network.Start
    turns |> should equal 3
    
[<Test>]                     
let mockTest () =
    let probabilities = Map [ ("Linux", 0.5); ("Windows", 0.5) ]
    let computer1 = Computer("Computer1", "Linux", probabilities)
    computer1.Infect
    computer1.EndTurn
    let computer2 = Computer("Computer2", "Windows", probabilities)
    let computer3 = Computer("Computer3", "Windows", probabilities)
    let rnd = Mock<System.Random>()
                         .Setup(fun x -> <@ x.NextDouble() @>)
                         .Returns(0.15)
                         .Create()
    let arr = [| [| true; true; false |]; [| true; true; true |]; [| false; true; true |] |]
    let network = Net([computer1; computer2; computer3],  Array2D.init 3 3 (fun i j -> arr[i][j]), rnd)
    let turns = network.Start
    turns |> should equal 3

    
[<Test>]                     
let stableTest () =
    let probabilities = Map [ ("Linux", 0.5); ("Windows", 0.5) ]
    let computer1 = Computer("Computer1", "Linux", probabilities)
    computer1.Infect
    computer1.EndTurn
    let computer2 = Computer("Computer2", "Windows", probabilities)
    let computer3 = Computer("Computer3", "Windows", probabilities)
    let network = Net([computer1; computer2; computer3],  Array2D.init 3 3 (fun _ _ -> false), System.Random())
    let turns = network.Start
    turns |> should equal 1
    
[<Test>]                     
let oneMoreStableTest () =
    let probabilities = Map [ ("Linux", 0.); ("Windows", 0.) ]
    let computer1 = Computer("Computer1", "Linux", probabilities)
    computer1.Infect
    computer1.EndTurn
    let computer2 = Computer("Computer2", "Windows", probabilities)
    let computer3 = Computer("Computer3", "Windows", probabilities)
    let network = Net([computer1; computer2; computer3],  Array2D.init 3 3 (fun _ _ -> true), System.Random())
    let turns = network.Start
    turns |> should equal 1