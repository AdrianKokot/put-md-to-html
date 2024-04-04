module modules.ContentReader

open modules.CommandLine
open System
open System.IO

let private readFile (filePath: string) = File.ReadAllText(filePath)
let private readConsoleInput () = Console.In.ReadToEnd()

let readContent (options: CommandLineOptions) =
    if options.readFromFile then
        if options.verbose then
            printf "Reading content from file: %s\n" options.filePath
            
        readFile options.filePath
    else if options.inputText <> "" then
        if options.verbose then
            printf "Reading content from arguments\n"
            
        options.inputText
    else
        if options.verbose then
            printf "Reading content interactively\n"
            
        readConsoleInput()