open System.Collections.Generic

type Phonebook(records: Dictionary<string, int>) =
    member this.Records with get () = records
    
    new () = Phonebook (Dictionary<string, int>())
    
    /// Finds by name number in phonebook
    member this.FindByName name = this.Records.Item name

    /// Finds by number user in phonebook
    member this.FindByNumber number = Seq.find (fun elem -> this.FindByName elem = number) this.Records.Keys

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
        System.IO.File.WriteAllLines (path, Seq.map (fun key -> key + " " + (this.FindByName key).ToString()) this.Records.Keys)
                                                                