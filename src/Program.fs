module MdToHtml.Program

open MdToHtml.Modules.HtmlRenderer
open MdToHtml.Modules.CommandLine
open MdToHtml.Modules.ContentReader
open MdToHtml.Modules.MarkdownParser
open System.IO

[<EntryPoint>]
let main argv =

    let options = parseCommandLineArguments (argv |> Array.toList)

    let exportToFile (filePath: string) (content: string) =
        File.WriteAllText(filePath, content)

    let htmlContent =
        options
        |> readContent
        |> parseMarkdown
        |> renderHtml options

    match options.outputFilePath with
    | Some path -> exportToFile path htmlContent
    | None -> printfn "%s" htmlContent

    0
