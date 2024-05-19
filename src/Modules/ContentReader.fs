module MdToHtml.Modules.ContentReader

open MdToHtml.Modules.CommandLine
open System
open System.IO

let private readFile (filePath: string) = File.ReadAllText(filePath)
let private readConsoleInput () = Console.In.ReadToEnd()

let private _readContent  (options: CommandLineOptions) =
                             if options.filePath <> None then                                     
                                 readFile options.filePath.Value
                             else if options.inputText <> "" then
                                 options.inputText
                             else                                     
                                 readConsoleInput()

let readContent (options: CommandLineOptions) =
    let content = _readContent options
    
    content.Split([|'\n'|]) |> Array.toList