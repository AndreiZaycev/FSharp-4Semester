module MiniCrowlerTest

open NUnit.Framework
open FsUnit
open MiniCrowler

[<Test>]
let checkTurnikmeniProtivKachkov () =
    let parsedPage = download "https://www.sovsport.ru/lifestyle/1014685-turnikmeny-protiv-kachkov-pochemu-oni-nenavidjat-drug-druga"
    Seq.length parsedPage |> should equal 6
    let listOfLinks = [
        "https://www.youtube.com/watch?v=_-Wj2Fb9ST4"
        "https://sovsport.ru/about"
        "https://sovsport.ru/advert"
        "https://sovsport.ru/careers"
        "https://sovsport.ru/confidential"
        "https://sovsport.ru/content"
    ]
    let len = [
        306818
        27033
        24769
        25029
        85843
        27725
    ]
    Seq.iteri
        (fun i elem -> elem |> should (equalWithin 10000) len[i])
        (Seq.map (fun (_, x: int option) -> x.Value) parsedPage)
    Seq.iteri
        (fun i elem -> elem |> should equal listOfLinks[i])
        (Seq.map fst parsedPage)
    