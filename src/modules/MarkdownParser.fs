module modules.MarkdownParser

///
/// Helpful terms F#:
/// * Active pattern
/// * Pipe forward operator
/// * Pattern matching
///
///
/// Helpful AST:
/// * [Entry level compiler - info about tokenization](https://medium.com/swlh/entry-level-compiler-ba7e91cffbb2)
/// * [Markdown AST](https://github.com/syntax-tree/mdast)
///

let (|ExampleActivePattern|_|) (starts: string, ends: string) (text: string) =
    if (text.StartsWith(starts)) then
        let endIdx = text.IndexOf(ends)

        if (endIdx > 0) then
            let content = text.Substring(starts.Length, endIdx - starts.Length)
            Some(content)
        else
            None
    else
        None


let parseMarkdown (lines: string list) =

    match lines.[0] with
    | ExampleActivePattern ("[", "]:") (content) -> content
    | _ -> ""
