module MdToHtml.Modules.CommandLine

type CommandLineOptions =
    { 
      filePath: string option
      inputText: string
      title: string
      outputFilePath: string option}


let rec private _parseCommandLineArguments argv options =
    match argv with
    | [] -> options
    
    | "-f" :: filePath :: tail
    | "--file" :: filePath :: tail ->
        _parseCommandLineArguments
            tail
            { options with
                filePath = Some filePath }
            
    | "-t" :: title :: tail
    | "--title" :: title :: tail ->
        _parseCommandLineArguments
            tail
            { options with title = title }

    | "-o" :: outputFilePath :: tail
    | "--output" :: outputFilePath :: tail ->
        _parseCommandLineArguments
            tail
            { options with outputFilePath = Some outputFilePath }
        
    | unknown :: _ when unknown.StartsWith("-") -> failwithf "Unknown option: %s" unknown
    | [ text ] -> { options with inputText = text }
    | _ :: tail -> _parseCommandLineArguments tail options


let parseCommandLineArguments argv =
    _parseCommandLineArguments
        argv
        { filePath = None
          inputText = ""
          title = "Markdown Document"
          outputFilePath = None}
