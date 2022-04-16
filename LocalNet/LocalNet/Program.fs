module LocalNet

open System

/// Implementation of the computer 
type Computer(name: string, os: string, probabilities: Map<string, float>) =
    let mutable infected = false   
    let mutable infectedThisTurn = false
    
    /// name of the computer
    member this.Name = name
    
    /// os of the computer
    member this.OS = os
    
    /// infects computer
    member this.Infect =
        infected <- true
        infectedThisTurn <- true
        
    /// gets the infection status
    member this.IsInfected = infected
    
    /// gets the infection status on this turn
    member this.IsInfectedThisTurn = infectedThisTurn
    
    /// ends turn 
    member this.EndTurn = infectedThisTurn <- false
    
    /// gets probability of infection of the specified os 
    member this.Probability = probabilities.Item os 

/// Implementation of network
type Net(computers: List<Computer>, mtx: bool[,], rnd: Random) =
    /// One turn of infection
    member this.turn =
        let mutable isStable = true
        List.iteri (fun i (computer: Computer) ->
            if not computer.IsInfected
            then
                List.iteri (fun j (otherComputer: Computer) ->
                    
                    let canInfect = rnd.NextDouble() <= otherComputer.Probability
                    if otherComputer.Probability > 0 && not canInfect && mtx[i, j] then isStable <- false
                    if mtx[i, j] && otherComputer.IsInfected && not otherComputer.IsInfectedThisTurn && canInfect
                    then
                        isStable <- false
                        computer.Infect) computers) computers
        List.iter (fun (computer: Computer) ->
            let message = if computer.IsInfected then "is infected" else "is not infected"
            printfn $"%s{computer.Name} %s{message}") computers
        List.iter (fun (computer: Computer) -> computer.EndTurn) computers
        isStable     

    /// starts infection
    member this.Start =
        let mutable isStable = false
        let mutable counter = 0
        while not isStable do
            counter <- counter + 1
            printfn $"Turn %A{counter}"
            isStable <- this.turn
        printfn "Network is stable"
        counter                
