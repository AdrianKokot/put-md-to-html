module Program

open System
open System.IO

open modules.CommandLine
open modules.ContentReader


[<EntryPoint>]
let main argv =

    let options = parseCommandLineArguments (argv |> Array.toList)
    let content = readContent options

    printf "%s" content

    0
