module MdToHtml.Modules.MarkdownParser

open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

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
    | Link of string * string * MarkdownAST list
    | Image of string * string * string
    | ListItem of MarkdownAST list
    | UnorderedList of MarkdownAST list
    | OrderedList of MarkdownAST list

// TODO: Refactor this method to accept a list of char instead of a string to avoid the conversion
let (|WrappedWith|_|) (starts: List<char>, ends: List<char>) (text: List<char>) =
    let startsString = new System.String(starts |> List.toArray)
    let endsString = new System.String(ends |> List.toArray)
    let textString = new System.String(text |> List.toArray)
    
    if textString.StartsWith(startsString) then
        let startsLength = starts.Length
        let endsLength = ends.Length
        
        if startsString = endsString then
            let id = textString.IndexOf(startsString, startsLength)
            
            if id >= 0 then
                let wrapped = textString.Substring(startsLength, id - startsLength) |> Seq.toList
                let rest = textString.Substring(id + startsLength, textString.Length - id - startsLength) |> Seq.toList
                Some(wrapped, rest)
            else
                None
        else
            let nextStart = textString.IndexOf(startsString, startsLength)
            
            if nextStart >= 0 then
                let id = textString.LastIndexOf(endsString, textString.Length - 1, textString.Length - nextStart - 1)
                
                if id >= 0 then
                    let wrapped = textString.Substring(startsLength, id - startsLength) |> Seq.toList
                    let rest = textString.Substring(id + endsLength, textString.Length - id - endsLength) |> Seq.toList
                    Some(wrapped, rest)
                else
                    None
            else
                let id = textString.LastIndexOf(endsString)
                
                if id >= 0 then
                    let wrapped = textString.Substring(startsLength, id - startsLength) |> Seq.toList
                    let rest = textString.Substring(id + endsLength, textString.Length - id - endsLength) |> Seq.toList
                    Some(wrapped, rest)
                else
                    None
    else
        None


// TODO: Simplify this method
let (|StartsWithRepeated|_|) (repeated: string) (text: string) =
    let rec loop i =
        if i = text.Length then i
        elif text[i] <> repeated[i % repeated.Length] then i
        else loop (i + 1)

    let n = loop 0

    if n = 0 || n % repeated.Length <> 0 then
        None
    else
        Some(n / repeated.Length, text.Substring(n, text.Length - n))

let (|Heading|_|) (line: string) =
    match line with
    | StartsWithRepeated "#" (level, text) -> Some(level, text.Substring(1))
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
        else
            None

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
        else
            None

let (|List|_|) (listItemRegex: string) (lines: string list) =
    let appendIfNotEmpty (acc: string list list) (currentListItemAcc: string list) =
        if currentListItemAcc.Length > 0 then
            (currentListItemAcc |> List.rev) :: acc
        else
            acc

    match lines with
    | [] -> None
    | line :: _ as mdList when Regex.IsMatch(line, listItemRegex) ->
        let rec loop (lines: string list) (currentListItemAcc: string list) (acc: string list list) =
            match lines with
            | [] -> Some((appendIfNotEmpty acc currentListItemAcc) |> List.rev, [])
            | "" :: rest -> Some((appendIfNotEmpty acc currentListItemAcc) |> List.rev, rest)
            | line :: rest ->
                let matches = Regex.Match(line, listItemRegex)

                if matches.Success then
                    loop rest [ line.Substring(matches.Length) ] (appendIfNotEmpty acc currentListItemAcc)
                else
                    loop rest (line.TrimStart() :: currentListItemAcc) acc

        loop mdList [] []
    | _ -> None

let (|OrderedList|_|) (lines: string list) =
    match lines with
    | List @"^\d+(\.|\)) " (list, rest) -> Some(list, rest)
    | _ -> None

let (|UnorderedList|_|) (lines: string list) =
    match lines with
    | List @"^[-+*] " (list, rest) -> Some(list, rest)
    | _ -> None

let (|HorizontalRule|_|) (line: string) =
    match line with
    | StartsWithRepeated "-" (amount, _)
    | StartsWithRepeated "*" (amount, _)
    | StartsWithRepeated "_" (amount, _) -> if amount >= 3 then Some() else None
    | _ -> None

let (|InlineCode|_|) (line: char list) =
    match line with
    | '`' :: _ ->
        match line with
        | WrappedWith (['`'], ['`']) (wrapped, rest) -> Some(wrapped, rest)
        | _ -> None
    | _ -> None


let (|Strong|_|) (line: char list) =
    match line with
    | '*' :: '*' :: _
    | '_' :: '_' :: _ ->
        match line  with
        | WrappedWith (['*'; '*'], ['*'; '*']) (wrapped, rest)
        | WrappedWith (['_'; '_'], ['*'; '*']) (wrapped, rest) ->
            Some(wrapped , rest)
        | _ -> None
    | _ -> None

let (|Image|_|) (line: char list) =
    match line with
    | '!' :: '[' :: _ ->
        match line with
        | WrappedWith (['!'; '['], [']']) (wrapped, rest) ->
            match rest with
            | WrappedWith (['('], [')']) (pathAndTitle, rest) ->
                let pathAndTitleStr = pathAndTitle |> List.toArray |> System.String.Concat
                let parts = pathAndTitleStr.Split([| ' ' |], 2)
                match parts with
                | [| path; title |] ->
                    Some(
                        wrapped,
                        path,
                        title.Trim([| '"' |]),
                        rest
                    )
                | [| path |] -> Some(wrapped, path, "", rest)
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let (|Link|_|) (line: char list) =
    match line with
    | '[' :: _ ->
        match line with
        | WrappedWith (['['], [']']) (wrapped, rest) ->
            match rest with
            | WrappedWith (['('], [')']) (urlAndTitle, rest) ->
                let urlAndTitleStr = urlAndTitle |> Array.ofList |> System.String.Concat
                let parts = urlAndTitleStr.Split([| ' ' |], 2)
                match parts with
                | [| url; title |] ->
                    Some(
                        wrapped,
                        url,
                        title.Trim([| '"' |]),
                        rest
                    )
                | [| url |] -> Some(wrapped, url, "", rest)
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let (|Emphasis|_|) (line: char list) =
    match line with
    | '*' :: _
    | '_' :: _ ->
        match line with
        | WrappedWith (['*'], ['*']) (wrapped, rest)
        | WrappedWith (['_'], ['_']) (wrapped, rest) ->
            Some(wrapped, rest)
        | _ -> None
    | _ -> None


let parseCharsAcc (acc: char list) =
    if acc.Length > 0 then
        [ Text(acc |> List.rev |> System.String.Concat) ]
    else
        []

let (|EscapableChar|_|) =
    function
    | '`'
    | '*'
    | '_'
    | '\\'
    | '{'
    | '}'
    | '['
    | ']'
    | '('
    | ')'
    | '#'
    | '+'
    | '-'
    | '.'
    | '!'
    | '|' as char -> Some(char)
    | _ -> None

let (|HtmlEntity|_|) =
    function
    | '>' -> Some([ ';'; 't'; 'g'; '&' ])
    | '<' -> Some([ ';'; 't'; 'l'; '&' ])
    | '&' -> Some([ ';'; 'p'; 'm'; 'a'; '&' ])
    | '"' -> Some([ ';'; 't'; 'o'; 'u'; 'q'; '&' ])
    | '\'' -> Some([ ';'; 's'; 'o'; 'p'; 'a'; '&' ])
    | _ -> None

let rec parseChars (line: char list) (acc: char list) =
    seq {
        match line with
        | [] -> yield! parseCharsAcc acc
        | ' ' :: ' ' :: [ '\r' ]
        | ' ' :: [ ' ' ] ->
            yield! parseCharsAcc acc
            yield LineBreak
        | '\\' :: EscapableChar(char) :: rest -> yield! parseChars rest (char :: acc)
        | '\\' :: HtmlEntity(chars) :: rest -> yield! parseChars rest (chars @ acc)
        | Strong(wrapped, rest) ->
            yield! parseCharsAcc acc
            yield Strong(parseChars wrapped [] |> List.ofSeq)
            yield! parseChars rest []
        | Emphasis(wrapped, rest) ->
            yield! parseCharsAcc acc
            yield Emphasis(parseChars wrapped [] |> List.ofSeq)
            yield! parseChars rest []
        | Link(wrapped, url, title, rest) ->
            yield! parseCharsAcc acc
            yield Link(url, title, parseChars wrapped [] |> List.ofSeq)
            yield! parseChars rest []
        | Image(alt, path, title, rest) ->
            yield! parseCharsAcc acc
            yield Image(path, title, alt |> System.String.Concat)
            yield! parseChars rest []
        | InlineCode(wrapped, rest) ->
            yield! parseCharsAcc acc
            yield InlineCode(wrapped|> List.toArray |> System.String.Concat)
            yield! parseChars rest []
        | c :: rest -> yield! parseChars rest (c :: acc)
    }

let rec parseBlock (line: string) =
    parseChars (line.ToCharArray() |> List.ofArray) [] |> List.ofSeq

let parseBlocksAcc (acc: string list) =
    if acc.Length > 0 then
        [ Paragraph(acc |> List.rev |> List.map parseBlock |> List.concat) ]
    else
        []

let rec parseBlocks (lines: string list) (acc: string list) =
    seq {
        match lines with
        | HorizontalRule :: rest ->
            yield! parseBlocksAcc acc
            yield HorizontalRule
            yield! parseBlocks rest []
        | CodeBlock(code, lang, rest) ->
            yield! parseBlocksAcc acc
            yield CodeBlock(lang, code)
            yield! parseBlocks rest []
        | BlockQuote(quoted, rest) ->
            yield! parseBlocksAcc acc
            yield BlockQuote(parseBlocks quoted [] |> List.ofSeq)
            yield! parseBlocks rest []
        | Heading(level, text) :: rest ->
            yield! parseBlocksAcc acc
            yield Header(level, parseBlock text)
            yield! parseBlocks rest []
        | OrderedList(list, rest) ->
            yield! parseBlocksAcc acc
            yield OrderedList(list |> List.map (fun a -> ListItem(parseBlocks a [] |> List.ofSeq)))
            yield! parseBlocks rest []
        | UnorderedList(list, rest) ->
            yield! parseBlocksAcc acc
            yield UnorderedList(list |> List.map (fun a -> ListItem(parseBlocks a [] |> List.ofSeq)))
            yield! parseBlocks rest []
        | ("" | "\r") :: rest ->
            yield! parseBlocksAcc acc
            yield! parseBlocks rest []
        | line :: rest -> yield! parseBlocks rest (line :: acc)
        | [] -> yield! parseBlocksAcc acc
    }

let parseMarkdown (lines: string list) = parseBlocks lines [] |> List.ofSeq
