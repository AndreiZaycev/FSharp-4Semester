module MiniCrowler
open System.Net.Http
open System.Text.RegularExpressions

/// Returns the size of the pages
let downloadAsync (url: string) =
    let regex = Regex("<a href=\"(https://[^\"]+)\">", RegexOptions.Compiled)
    
    let downloadPageAsync (_url: string) =
        async {
            try
                use client = new HttpClient()
                let! string = Async.AwaitTask (client.GetStringAsync(_url))
                return Some string
            with
            | _ -> return None 
        }

    let mainPage = Async.RunSynchronously (downloadPageAsync url)

    match mainPage with
    | Some page ->
        let matchedPages = regex.Matches(page)
        [ for m in matchedPages -> downloadPageAsync m.Groups.[1].Value ]
        |> Async.Parallel |> Async.RunSynchronously |> Seq.distinct 
        |> Seq.zip [for m in matchedPages -> m.Groups.[1].Value ]
        |> Seq.map (fun (url, x) ->
            match x with
            | Some a -> (url, Some a.Length)
            | None -> (url, None))
    | _ -> Seq.empty

/// Gets the result to print 
let getDownloadedInfoToPrint url =
    let parsedPages = downloadAsync url
    Seq.map (fun (parsedPage: string * int option) -> 
        match parsedPage with
        | url, Some x -> $"%s{url} - %d{x} symbols"
        | _, None -> "Cannot download page") parsedPages

/// Prints result    
let print url = printfn $"%A{getDownloadedInfoToPrint url}"
