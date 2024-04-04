module modules.CommandLine

type CommandLineOptions =
    { verbose: bool
      readFromFile: bool
      filePath: string
      inputText: string }

let rec private _parseCommandLineArguments argv options =
    match argv with
    | [] -> options
    | "-v" :: tail -> _parseCommandLineArguments tail { options with verbose = true }
    | "-f" :: filePath :: tail ->
        _parseCommandLineArguments
            tail
            { options with
                readFromFile = true
                filePath = filePath }
    | unknown :: _ when unknown.StartsWith("-") -> failwithf "Unknown option: %s" unknown
    | [ text ] -> { options with inputText = text }
    | _ :: tail -> _parseCommandLineArguments tail options


let parseCommandLineArguments argv =
    _parseCommandLineArguments
        argv
        { verbose = false
          readFromFile = false
          filePath = ""
          inputText = "" }
