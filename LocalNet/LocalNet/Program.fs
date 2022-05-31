module LocalNet

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
   
/// Implementation of virus that can infect computer  
type Virus(Name: string, Random: unit -> float, Linux: float, Windows: float, MacOS: float, Infected: List<Computer>) =
    let mutable infected = Infected
    member this.Infect computer = infected <- computer :: infected
    member this.GetInfected() = infected
    member this.Name = Name 
    member this.Random = Random 
    member this.Linux = Linux 
    member this.Windows = Windows 
    member this.MacOS = MacOS 
    member this.Infected = Infected

/// Implementation of network
type Net(computers: List<Computer>, mtx: bool[,], viruses: List<Virus>) =
    
    /// One turn of infection
    member this.Turn() =        
        let mutable infectedThisTurn = List.empty
        
        // Gets the infected computers on the previous turns 
        let infectedOnPreviousTurns =
            let arr = Array.zeroCreate computers.Length
            List.iter (fun (virus: Virus) ->
                          for computer in virus.GetInfected() do
                              let index = List.findIndex (fun elem -> elem = computer) computers
                              arr[index] <- true)
                      viruses
            arr
         
        // Checks computer's infection    
        let isNotInfected(computer: Computer) =
            let index = List.findIndex (fun comp -> comp = computer) computers
            not (List.exists (fun elem -> elem = computer) infectedThisTurn) && not infectedOnPreviousTurns.[index]
            
        let mutable isStable = true
          
        // Finds all infected connections to the computer and tries to infect him 
        let tryToInfectComputer(computer: Computer) =
            // Index of computer 
            let index = List.findIndex (fun comp -> comp = computer) computers
            for i in 0 .. computers.Length - 1 do
                let com = computers.[i]
                if com <> computer && mtx[index, i] && infectedOnPreviousTurns.[i]
                   && not (List.contains com infectedThisTurn)
                then
                    // Gets all viruses that can potentially infect our computer 
                    let virusesCanPotentiallyInfectComputer = 
                        List.filter (fun (virus: Virus) -> List.contains com (virus.GetInfected())) viruses
                    let condition (v: Virus) discriminatedUnionNumber =
                        let virusOSProbability = 
                            match discriminatedUnionNumber with
                            | 0 -> v.Linux
                            | 1 -> v.Windows
                            | _ -> v.MacOS
                        let virusRandom = v.Random()
                        if virusOSProbability > 0 && virusRandom > virusOSProbability then isStable <- false
                        if virusRandom <= virusOSProbability && not (List.contains computer infectedThisTurn)
                            then
                                isStable <- false
                                v.Infect computer
                                // современные проблемы требуют современных решений 
                                infectedThisTurn <-  computer :: infectedThisTurn
                    List.iter
                        (fun (virus: Virus) ->
                            match computer.OS with
                            | Linux -> condition virus 0 
                            | Windows -> condition virus 1 
                            | MacOS -> condition virus 2)
                        virusesCanPotentiallyInfectComputer
                           
        for i in 0 .. computers.Length - 1 do
            let computer = computers[i] 
            if isNotInfected(computer) then tryToInfectComputer(computer)          
            
        for j in 0 .. computers.Length - 1 do 
            let message =
                if infectedOnPreviousTurns.[j] || (List.exists (fun elem -> elem = computers.[j]) infectedThisTurn)
                then "is infected" else "is not infected"
            printfn $"%s{computers.[j].Name} %s{message}"  
        
        isStable     

    /// starts infection
    member this.Start() =
        let mutable isStable = false
        let mutable counter = 0
        while not isStable do
            counter <- counter + 1
            printfn $"Turn %A{counter}"
            isStable <- this.Turn()
        printfn "Network is stable"
        counter