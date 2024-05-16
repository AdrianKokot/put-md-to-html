module MdToHtml.Modules.CommandLine

type CommandLineOptions =
    { readFromFile: bool
      filePath: string
      inputText: string
      title: string}

// TODO: Export to file

let rec private _parseCommandLineArguments argv options =
    match argv with
    | [] -> options
    
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
        { readFromFile = false
          filePath = ""
          inputText = ""
          title = "Markdown Document"}
