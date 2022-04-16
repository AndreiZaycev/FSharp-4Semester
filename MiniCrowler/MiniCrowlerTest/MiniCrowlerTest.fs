module MiniCrowlerTest

open NUnit.Framework
open FsUnit
open MiniCrowler

[<Test>]
let checkMyGithub () =
    let parsedPage = downloadAsync "https://github.com/AndreiZaycev/"
    Seq.length parsedPage |> should equal 3
    let len = [| (145000, 160000); (158000, 165000); (155000, 165000) |]
    Seq.iteri (fun i elem ->
        elem |> should be (greaterThan (fst len[i]))
        elem |> should be (lessThan (snd len[i]))) (Seq.map (fun (_, x: int option) -> x.Value) parsedPage)
    
[<Test>]
let checkPrinter() =
    let parsedPage = getDownloadedInfoToPrint "https://github.com/AndreiZaycev/"
    Seq.length parsedPage |> should equal 3
    let links =
        [
        "https://docs.github.com/en/articles/blocking-a-user-from-your-personal-account";
        "https://docs.github.com/en/articles/reporting-abuse-or-spam";
        "https://docs.github.com/categories/setting-up-and-managing-your-github-profile"
        ]
    /// поиск подстроки в строке  
    let find first second =
        Seq.fold2 (fun acc elem elem1 -> if elem = elem1 then acc else false) true first second 

    Seq.iteri (fun i elem -> find links[i] elem |> should equal true) parsedPage