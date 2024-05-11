module MdToHtml.Modules.MarkdownParser

open Microsoft.FSharp.Collections

// TODO: Links
// TODO: Formatted links (emphasis/strong around link, code inside link)
// TODO: Images
// TODO: Escaping characters
// TODO: Proper handling linebreaks
// TODO: Refactor paragraphs (paragraph next to paragraph should be linebreak not 2 paragraphs unless there's blank line in between them)
// TODO: Lists

type MarkdownAST =
    | Header of int * MarkdownAST list
    | Paragraph of MarkdownAST list 
    | Emphasis of MarkdownAST list
    | Strong of MarkdownAST list
    | BlockQuote of MarkdownAST list
    | Text of string
    | LineBreak
    | CodeBlock of string * string list
    | InlineCode of string
    | HorizontalRule

let (|WrappedWith|_|) (starts:string, ends:string) (text:string) =
    if text.StartsWith(starts) then
      let id = text.IndexOf(ends, starts.Length)
      if id >= 0 then
        let wrapped = text.Substring(starts.Length, id - starts.Length)
        let rest = text.Substring(id + ends.Length, text.Length - id - ends.Length)
        Some(wrapped, rest)
      else None
    else None

let (|StartsWithRepeated|_|) (repeated:string) (text:string) =
    let rec loop i =
      if i = text.Length then i
      elif text.[i] <> repeated.[i % repeated.Length] then i
      else loop (i + 1)

    let n = loop 0
    if n = 0 || n % repeated.Length <> 0 then None
    else Some(n/repeated.Length, text.Substring(n, text.Length - n))
    
let (|Heading|_|) (line: string) =
    match line with
    | StartsWithRepeated("#") (level, text) -> Some(level, text.Substring(1))
    | _ -> None
   
let (|BlockQuote|_|) (lines: string list) =
    match lines with
    | [] -> None
    | line :: _ as mdQuote ->
        if line.StartsWith(">") then
            let rec loop (lines: string list) (acc: string list) = 
                match lines with
                | [] -> Some(acc |> List.rev, [])
                | line :: rest ->
                    if line.StartsWith(">") then
                        loop rest (line.Substring(1).TrimStart() :: acc)
                    else
                        Some(acc |> List.rev, line :: rest)
                        
            loop mdQuote []
        else None
        
let (|CodeBlock|_|) (lines: string list) =
    match lines with
    | [] -> None
    | line :: rest ->
        if line.StartsWith("```") then
            let lang = line.Substring(3)
            
            let rec loop (lines: string list) (acc: string list) = 
                match lines with
                | [] -> Some(acc |> List.rev, lang, [])
                | line :: rest ->
                    if line.StartsWith("```") then
                        Some(acc |> List.rev, lang, rest)
                    else
                        loop rest (line :: acc)
                        
            loop rest []
        else None
        
let (|HorizontalRule|_|) (line: string) =
    match line with
    | StartsWithRepeated("-") (amount, _) 
    | StartsWithRepeated("*") (amount, _) 
    | StartsWithRepeated("_") (amount, _) ->
        if amount >= 3 then Some()
        else None
    | _ -> None

let (|InlineCode|_|) (line: char list) =
    match line with
    | '`'::_ ->
        match line |> Array.ofList |> System.String.Concat with
        | WrappedWith("`", "`") (wrapped, rest) -> Some(wrapped, rest.ToCharArray() |> List.ofArray)
        | _ -> None
    | _ -> None

let (|Strong|_|) (line: char list) =
    match line with
    | '*'::'*'::_ 
    | '_'::'_'::_ ->
        match line |> Array.ofList |> System.String.Concat with
        | WrappedWith("**", "**") (wrapped, rest) 
        | WrappedWith("__", "__") (wrapped, rest) -> Some(wrapped.ToCharArray() |> List.ofArray, rest.ToCharArray() |> List.ofArray)
        | _ -> None
    | _ -> None

let (|Emphasis|_|) (line: char list) =
    match line with
    | '*'::_ 
    | '_'::_ ->
        match line |> Array.ofList |> System.String.Concat with
        | WrappedWith("*", "*") (wrapped, rest) 
        | WrappedWith("_", "_") (wrapped, rest) -> Some(wrapped.ToCharArray() |> List.ofArray, rest.ToCharArray() |> List.ofArray)
        | _ -> None
    | _ -> None
    

let parseCharsAcc (acc: char list) =
    if acc.Length > 0 then
        [Text(new string(acc |> List.rev |> List.toArray))]
    else
        []

let rec parseChars (line: char list) (acc: char list) = seq {
    match line with
    | [] ->
        yield! parseCharsAcc acc
    | ' '::' '::'\r'::'\n':: rest
    | ' '::' '::('\n' | '\r'):: rest ->
        yield LineBreak
        yield! parseChars rest []
    | Strong(wrapped, rest) ->
        yield! parseCharsAcc acc
        yield Strong(parseChars wrapped [] |> List.ofSeq)
        yield! parseChars rest []
    | Emphasis(wrapped, rest) ->
        yield! parseCharsAcc acc
        yield Emphasis(parseChars wrapped [] |> List.ofSeq)
        yield! parseChars rest []
    | InlineCode(wrapped, rest) ->
        yield! parseCharsAcc acc
        yield InlineCode(wrapped)
        yield! parseChars rest []
    | c::rest ->
        yield! parseChars rest (c::acc)
}    

let rec parseBlock (line: string) =
    parseChars (line.ToCharArray() |> List.ofArray) [] |> List.ofSeq
    
let rec parseBlocks (lines: string list) = seq {
    match lines with
    | [] -> ()
    | HorizontalRule :: rest ->
        yield HorizontalRule
        yield! parseBlocks(rest)
    | CodeBlock(code, lang, rest) ->
        yield CodeBlock(lang, code)
        yield! parseBlocks(rest)
    | BlockQuote(quoted, rest) ->
        yield BlockQuote(parseBlocks quoted |> List.ofSeq)
        yield! parseBlocks(rest)
    | Heading(level, text) :: rest ->
        yield Header(level, parseBlock text)
        yield! parseBlocks(rest)
    | line :: rest ->
        yield Paragraph(parseBlock line)
        yield! parseBlocks(rest)
}

let parseMarkdown (lines: string list) =
    parseBlocks(lines) |> List.ofSeq