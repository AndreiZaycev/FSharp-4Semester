module MiniCrowler
open System.Net.Http
open System.Text.RegularExpressions

/// Returns the size of the pages
let downloadAsync (url: string) =
    let urlRegex = Regex("<a href=\"(https://[^\"]+)\">", RegexOptions.Compiled)
    let downloadPageAsync (_url: string) =
        async {
            try
                use client = new HttpClient()
                let! string = Async.AwaitTask (client.GetStringAsync(_url))
                return Some string
            with
            | _ -> return None 
        }

    let mainPageResult = downloadPageAsync url |> Async.RunSynchronously

    match mainPageResult with
    | Some page ->
        let matches = urlRegex.Matches(page)
        [ for m in matches -> downloadPageAsync m.Groups.[1].Value ]
        |> Async.Parallel |> Async.RunSynchronously |> Seq.distinct 
        |> Seq.zip [for m in matches -> m.Groups.[1].Value ]
        |> Seq.map (fun (url, x) ->
            match x with
            | Some a -> (url, Some a.Length)
            | None -> (url, None))
    | _ -> Seq.empty

/// Gets the result to print 
let getDownloadedInfoToPrint url =
    let results = downloadAsync url
    Seq.map (fun (result: string * int option) -> 
        match result with
        | url, Some x -> $"%s{url} - %d{x} symbols"
        | _, None -> "Cannot download page") results

/// Prints result    
let print url = printfn $"%A{getDownloadedInfoToPrint url}"
