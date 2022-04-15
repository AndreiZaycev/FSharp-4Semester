module PhonebookTests

open NUnit.Framework
open FsCheck
open FsUnit
open Program

[<Test>]
let TestAddNumberAndGet () =
    let book = Phonebook()
    book.Add "Андрюха 228" 228
    book.FindByName "Андрюха 228" |> should equal 228
    book.FindByNumber 228 |> should equal "Андрюха 228"
    (fun () -> book.FindByName "Ваня качок гриф 20кг монстр" |> ignore) |> should throw typeof<System.Collections.Generic.KeyNotFoundException>
    (fun () -> book.FindByNumber 1337 |> ignore) |> should throw typeof<System.Collections.Generic.KeyNotFoundException>
    
[<Test>]
let TestWriteAndReadWorksCorrectly () =
    let book = Phonebook ()
    book.Add "Андрюха" 1337 
    book.Add "Санёк" 228
    book.Add "Ванёк" 777

    book.SaveToFile "Andryuha228.txt"

    let bookFromFile = book.FromFile "Andryuha228.txt"
    
    book.Records |> should equivalent bookFromFile.Records