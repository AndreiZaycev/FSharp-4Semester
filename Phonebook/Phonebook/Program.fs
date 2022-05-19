module Program 
open System.Collections.Generic
open Microsoft.FSharp.Collections

type Phonebook(records: seq<string * int>) =
    let mutable record = records 
    member this.Records
        with get () = record
        and set(value) = record <- value 
    
    new () = Phonebook (Seq.empty)
    
    /// Finds by name number in phonebook
    member this.FindByName name =
        let a = Seq.map snd (Seq.filter (fun (key, _) -> key = name) this.Records)
        if Seq.isEmpty a
        then "Person with this name is not found"
        else (Seq.head a).ToString() 

    /// Finds by number user in phonebook
    member this.FindByNumber number =
        let a = Seq.map fst (Seq.filter (fun (_, value) -> value = number) this.Records)
        if Seq.isEmpty a
        then "Person with this number is not found"
        else Seq.head a 
       

    /// Adds record to a phonebook
    member this.Add name number = this.Records <- Seq.append this.Records [(name, number)]

    /// Get phonebook from specified file 
    member this.FromFile path =
        let book = Phonebook()
        System.IO.File.ReadLines path |> Seq.iter (fun elem ->
            let a = elem.Split(" ")
            book.Add a[0] (int(a[1])))
        book

    /// Saves phonebook to a specified file 
    member this.SaveToFile path =
        System.IO.File.WriteAllLines (path, Seq.map (fun (key, _) -> key + " " + (this.FindByName key).ToString()) this.Records)
                                                                