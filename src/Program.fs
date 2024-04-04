module Program

open modules.CommandLine
open modules.ContentReader
open modules.MarkdownParser

[<EntryPoint>]
let main argv =

    let options = parseCommandLineArguments (argv |> Array.toList)
    let content = readContent options

    let md = parseMarkdown content

    printf $"%A{md}"

    0