module Program
open System.Collections.Generic

type Phonebook(records: List<string * int>) =
    member this.Records with get () = records
    
    new () = Phonebook (List<string * int>())
    
    /// Finds by name number in phonebook
    member this.FindByName name = Seq.find (fun (key, value) -> key = name) this.Records |> snd 

    /// Finds by number user in phonebook
    member this.FindByNumber number = Seq.find (fun (key, value) -> value = number) this.Records |> fst

    /// Adds record to a phonebook
    member this.Add name number = this.Records.Add (name, number)

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
                                                                