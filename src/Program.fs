module MdToHtml.Program

open MdToHtml.Modules.HtmlRenderer
open MdToHtml.Modules.CommandLine
open MdToHtml.Modules.ContentReader
open MdToHtml.Modules.MarkdownParser

[<EntryPoint>]
let main argv =

    let options = parseCommandLineArguments (argv |> Array.toList)

    options
    |> readContent
    |> parseMarkdown
    |> renderHtml options
    |> printfn "%s"

    0
