module PhonebookTests

open NUnit.Framework
open FsUnit
open Program

[<Test>]
let TestAddNumberAndGet () =
    let book = Phonebook()
    book.Add "Андрюха 228" 228
    book.FindByName "Андрюха 228" |> should equal "228"
    book.FindByNumber 228 |> should equal "Андрюха 228"
    book.FindByName "Ваня качок гриф 20кг монстр" |> should equal "Person with this name is not found"
    book.FindByNumber 1337 |> should equal "Person with this number is not found"
    
[<Test>]
let TestWriteAndReadWorksCorrectly () =
    let book = Phonebook ()
    book.Add "Андрюха" 1337 
    book.Add "Санёк" 228
    book.Add "Ванёк" 777

    book.SaveToFile "Andryuha228.txt"

    let bookFromFile = book.FromFile "Andryuha228.txt"
    
    book.Records |> should equivalent bookFromFile.Records