module MiniCrowler
open System.Net.Http
open System.Text.RegularExpressions

/// Returns the size of the pages
let download (url: string) =
    let regex = Regex("<a href=\"(https://[^\"]+)\">", RegexOptions.Compiled)
    
    let downloadPageAsync (_url: string) =
        let httpClient = new HttpClient()
        async {
            try
                use client = httpClient
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
        |> Async.Parallel |> Async.RunSynchronously 
        |> Seq.zip [for m in matchedPages -> m.Groups.[1].Value ]
        |> Seq.map (fun (url, x) ->
            match x with
            | Some a -> (url, Some a.Length)
            | None -> (url, None))
    | _ -> Seq.empty

/// Gets the result to print 
let getDownloadedInfoToPrint url =
    let parsedPages = download url
    Seq.map (function
        | url, Some x -> $"%s{url} - %d{x} symbols"
        | _, None -> "Cannot download page") parsedPages

/// Prints result    
let print url = printfn $"%A{getDownloadedInfoToPrint url}"
