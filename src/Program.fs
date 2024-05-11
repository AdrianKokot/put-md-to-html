module Program

open modules.HtmlRenderer
open modules.CommandLine
open modules.ContentReader
open modules.MarkdownParser

[<EntryPoint>]
let main argv =

    let options = parseCommandLineArguments (argv |> Array.toList)

    options
    |> readContent
    |> parseMarkdown
    |> renderHtml options
    |> printfn "%s"

    0
