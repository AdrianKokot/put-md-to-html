module MdToHtml.Modules.MarkdownParser

open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

// TODO: Proper handling linebreaks
// TODO: Refactor paragraphs (paragraph next to paragraph should be linebreak not 2 paragraphs unless there's blank line in between them)

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

let (|WrappedWith|_|) (starts: string, ends: string) (text: string) =
    if text.StartsWith(starts) then

        if starts = ends then
            let id = text.IndexOf(starts, starts.Length)

            if id >= 0 then

                let wrapped = text.Substring(starts.Length, id - starts.Length)
                let rest = text.Substring(id + starts.Length, text.Length - id - starts.Length)
                Some(wrapped, rest)
            else
                None
        else
            let nextStart = text.IndexOf(starts, starts.Length)

            if nextStart >= 0 then

                let id = text.LastIndexOf(ends, text.Length - 1, text.Length - nextStart - 1)

                if id >= 0 then
                    let wrapped = text.Substring(starts.Length, id - starts.Length)
                    let rest = text.Substring(id + ends.Length, text.Length - id - ends.Length)
                    Some(wrapped, rest)
                else
                    None
            else
                let id = text.LastIndexOf(ends)

                if id >= 0 then
                    let wrapped = text.Substring(starts.Length, id - starts.Length)
                    let rest = text.Substring(id + ends.Length, text.Length - id - ends.Length)
                    Some(wrapped, rest)
                else
                    None
    else
        None

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

let (|List|_|) (listItemRegex: string) (lines: string list ) =
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
                    loop rest [line.Substring(matches.Length)] (appendIfNotEmpty acc currentListItemAcc)
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
        match line |> Array.ofList |> System.String.Concat with
        | WrappedWith ("`", "`") (wrapped, rest) -> Some(wrapped, rest.ToCharArray() |> List.ofArray)
        | _ -> None
    | _ -> None

let (|Strong|_|) (line: char list) =
    match line with
    | '*' :: '*' :: _
    | '_' :: '_' :: _ ->
        match line |> Array.ofList |> System.String.Concat with
        | WrappedWith ("**", "**") (wrapped, rest)
        | WrappedWith ("__", "__") (wrapped, rest) ->
            Some(wrapped.ToCharArray() |> List.ofArray, rest.ToCharArray() |> List.ofArray)
        | _ -> None
    | _ -> None

let (|Link|_|) (line: char list) =
    match line with
    | '[' :: _ ->
        match line |> Array.ofList |> System.String.Concat with
        | WrappedWith ("[", "]") (wrapped, rest) ->
            match rest with
            | WrappedWith ("(", ")") (urlAndTitle, rest) ->
                match urlAndTitle.Split(" ", 2) with
                | [| url; title |] ->
                    Some(
                        wrapped.ToCharArray() |> List.ofArray,
                        url,
                        title.Trim([| '"' |]),
                        rest.ToCharArray() |> List.ofArray
                    )
                | [| url |] -> Some(wrapped.ToCharArray() |> List.ofArray, url, "", rest.ToCharArray() |> List.ofArray)
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let (|Image|_|) (line: char list) =
    match line with
    | '!' :: '[' :: _ ->
        match line |> Array.ofList |> System.String.Concat with
        | WrappedWith ("![", "]") (wrapped, rest) ->
            match rest with
            | WrappedWith ("(", ")") (pathAndTitle, rest) ->
                match pathAndTitle.Split(" ", 2) with
                | [| path; title |] ->
                    Some(
                        wrapped.ToCharArray() |> List.ofArray,
                        path,
                        title.Trim([| '"' |]),
                        rest.ToCharArray() |> List.ofArray
                    )
                | [| url |] -> Some(wrapped.ToCharArray() |> List.ofArray, url, "", rest.ToCharArray() |> List.ofArray)
                | _ -> None
            | _ -> None
        | _ -> None
    | _ -> None

let (|Emphasis|_|) (line: char list) =
    match line with
    | '*' :: _
    | '_' :: _ ->
        match line |> Array.ofList |> System.String.Concat with
        | WrappedWith ("*", "*") (wrapped, rest)
        | WrappedWith ("_", "_") (wrapped, rest) ->
            Some(wrapped.ToCharArray() |> List.ofArray, rest.ToCharArray() |> List.ofArray)
        | _ -> None
    | _ -> None


let parseCharsAcc (acc: char list) =
    if acc.Length > 0 then
        [ Text(new string (acc |> List.rev |> List.toArray)) ]
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
        // | '\r' :: '\n' :: rest
        // | ('\n' | '\r') :: rest
        // | ' ' :: ' ' :: '\r' :: '\n' :: rest
        // | ' ' :: ' ' :: ('\n' | '\r') :: rest ->
        //     yield LineBreak
        //     yield! parseChars rest []
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
            yield InlineCode(wrapped)
            yield! parseChars rest []
        | c :: rest -> yield! parseChars rest (c :: acc)
    }

let rec parseBlock (line: string) =
    parseChars (line.ToCharArray() |> List.ofArray) [] |> List.ofSeq

let rec parseBlocks (lines: string list) =
    seq {
        match lines with
        | [] -> ()
        | HorizontalRule :: rest ->
            yield HorizontalRule
            yield! parseBlocks rest
        | CodeBlock(code, lang, rest) ->
            yield CodeBlock(lang, code)
            yield! parseBlocks rest
        | BlockQuote(quoted, rest) ->
            yield BlockQuote(parseBlocks quoted |> List.ofSeq)
            yield! parseBlocks rest
        | Heading(level, text) :: rest ->
            yield Header(level, parseBlock text)
            yield! parseBlocks rest
        | OrderedList(list, rest) ->
            yield OrderedList(list |> List.map (fun a -> ListItem(a |> parseBlocks |> List.ofSeq)))
            yield! parseBlocks rest
        | UnorderedList(list, rest) ->
            yield UnorderedList(list |> List.map (fun a -> ListItem(a |> parseBlocks |> List.ofSeq)))
            yield! parseBlocks rest
        | line :: rest ->
            yield Paragraph(parseBlock line)
            yield! parseBlocks rest
    }

let parseMarkdown (lines: string list) = lines |> parseBlocks |> List.ofSeq
