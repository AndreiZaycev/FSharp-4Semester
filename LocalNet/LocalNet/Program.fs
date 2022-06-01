module LocalNet

open System.Collections.Generic

/// Implementation of computer's OS 
type OS =
    | Linux
    | Windows
    | MacOS
     
/// Implementation of the computer 
type Computer(name: string, os: OS) =  
    /// name of the computer
    member this.Name = name 
    /// os of the computer
    member this.OS = os
 
/// Решил так сделать, чтобы они явно не передавались в Virus  
let mutable matrix = Array2D.zeroCreate 0 0 
let mutable computers = Array.empty
  
/// Implementation of Virus that can infects computers   
type Virus(firstInfected: Computer[], infectionChance: OS -> float, random: unit -> float) =
    let infectedComputers = HashSet(firstInfected)

    let getNeighbours computer =
        let indexOfComputer = Array.findIndex (fun (comp: Computer) -> computer = comp) computers
        let slice =
            matrix[indexOfComputer, *]
            |> Array.mapi (fun i hasConnection -> (i, hasConnection))
            |> Array.filter snd
            |> Array.map (fun (i, _) -> computers.[i])
        slice 
    

    let calculateInfectionCandidates lastInfected =
        lastInfected
        |> Seq.map getNeighbours  
        |> Seq.concat
        |> Seq.distinct
        |> Seq.filter (not << infectedComputers.Contains)
        |> Seq.filter (fun x -> infectionChance x.OS > 0)

    let mutable infectionCandidates = calculateInfectionCandidates infectedComputers

    member x.InfectedComputers = infectedComputers

    member x.AbleToInfect = not (Seq.isEmpty infectionCandidates)

    member x.SpreadInfection() =
        let newInfected = List()

        for computer in infectionCandidates do
            if random() < infectionChance computer.OS then
                newInfected.Add(computer)

        infectedComputers.UnionWith(newInfected)
        infectionCandidates <- calculateInfectionCandidates newInfected


/// Implementation of plague inc 
type PlagueInc(comps: Computer[], mtx: bool[,], viruses: Virus[]) =
    let rec play viruses counter =
        printfn $"turn %A{counter}"
        let viruses = viruses |> Seq.filter (fun (v: Virus) -> v.AbleToInfect)
        if Seq.isEmpty viruses
        then counter 
        else
            for virus in viruses do
                virus.SpreadInfection()
            play viruses (counter + 1)
        
    member x.Play() =
        computers <- comps
        matrix <- mtx 
        play viruses 1 
                          