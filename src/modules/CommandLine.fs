module modules.CommandLine

type CommandLineOptions =
    { verbose: bool
      readFromFile: bool
      filePath: string
      inputText: string
      title: string}

let rec private _parseCommandLineArguments argv options =
    match argv with
    | [] -> options
    | "-v" :: tail 
    | "--verbose" :: tail -> _parseCommandLineArguments tail { options with verbose = true }
    
    | "-f" :: filePath :: tail
    | "--file" :: filePath :: tail ->
        _parseCommandLineArguments
            tail
            { options with
                readFromFile = true
                filePath = filePath }
            
    | "-t" :: title :: tail
    | "--title" :: title :: tail ->
        _parseCommandLineArguments
            tail
            { options with title = title }
        
    | unknown :: _ when unknown.StartsWith("-") -> failwithf "Unknown option: %s" unknown
    | [ text ] -> { options with inputText = text }
    | _ :: tail -> _parseCommandLineArguments tail options


let parseCommandLineArguments argv =
    _parseCommandLineArguments
        argv
        { verbose = false
          readFromFile = false
          filePath = ""
          inputText = ""
          title = "Markdown Document"}
