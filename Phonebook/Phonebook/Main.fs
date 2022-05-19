open Program

module Main =
    
    open Argu
    open System
    type CLIArguments =
        | Add of name:string * number:int
        | Exit 
        | FindName of name:string 
        | FindPhoneNumber of number:int 
        | SaveBook of path:string
        | ReadBook of path:string 
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Add _ -> "Add new record to PhoneBook"
                | Exit _ -> "Exit"
                | FindName _ -> "Find phone number by name"
                | FindPhoneNumber _ -> "Find name by phone"
                | SaveBook _ -> "Save book to file"
                | ReadBook _ -> "Read book from file"    

    let main() =
        let mutable phoneBook = Phonebook()
        let mutable cnt = 0
        while cnt <> 1 do
            try 
                printfn "Input command"
                let argv = Console.ReadLine().Split(" ")
                let parser = ArgumentParser<CLIArguments>(programName = "PhoneBook")
                match parser.ParseCommandLine(inputs = argv, raiseOnUsage = true) with
                | p when p.Contains(Add) ->
                    let name, number = p.GetResult Add
                    phoneBook.Add name number
                    printfn "Record was added"
                | p when p.Contains(FindName) -> printfn $"{phoneBook.FindByName (p.GetResult FindName)}"
                | p when p.Contains(FindPhoneNumber) -> printfn $"{phoneBook.FindByNumber (p.GetResult FindPhoneNumber)}"
                | p when p.Contains(SaveBook) ->
                    phoneBook.SaveToFile (p.GetResult SaveBook)
                    printfn "Book was saved"
                | p when p.Contains(ReadBook) ->
                    phoneBook <- phoneBook.FromFile (p.GetResult ReadBook)
                    printfn "Book was read"
                | _ -> printfn $"%s{parser.PrintUsage()}"
            with
            | :? ArguParseException as ex ->
                printfn $"%s{ex.Message}"
    main()                             