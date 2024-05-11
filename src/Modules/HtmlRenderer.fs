﻿module MdToHtml.Modules.HtmlRenderer

open MdToHtml.Modules.CommandLine
open MdToHtml.Modules.MarkdownParser

let CSS = System.IO.File.ReadAllText("assets/style.css")

let HTML_HEADER= sprintf "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><style>%s</style><title>%s</title></head><body>" CSS
let HTML_FOOTER = "</body></html>"

let rec renderHtmlSeq (ast: MarkdownAST list) = seq {
    match ast with
    | [] -> ()
    | MarkdownAST.Text(content) :: rest ->
        yield content
        yield! renderHtmlSeq rest
    | MarkdownAST.HorizontalRule :: rest ->
        yield "<hr>"
        yield! renderHtmlSeq rest
    | MarkdownAST.Paragraph(content) :: rest ->
        yield "<p>"
        yield! renderHtmlSeq content
        yield "</p>"
        yield! renderHtmlSeq rest
    | MarkdownAST.Header(level, content) :: rest ->
        yield $"<h%d{level}>"
        yield! renderHtmlSeq content
        yield $"</h%d{level}>"
        yield! renderHtmlSeq rest
    | MarkdownAST.Emphasis(content) :: rest ->
        yield "<em>"
        yield! renderHtmlSeq content
        yield "</em>"
        yield! renderHtmlSeq rest
    | MarkdownAST.Strong(content) :: rest ->
        yield "<strong>"
        yield! renderHtmlSeq content
        yield "</strong>"
        yield! renderHtmlSeq rest
    | MarkdownAST.BlockQuote(content) :: rest ->
        yield "<blockquote>"
        yield! renderHtmlSeq content
        yield "</blockquote>"
        yield! renderHtmlSeq rest
    | MarkdownAST.CodeBlock(language, content) :: rest ->
        yield $"<pre data-lang=\"%s{language}\">"
        yield "<code>"
        yield String.concat "\n" content
        yield "</code>"
        yield "</pre>"
        yield! renderHtmlSeq rest
    | MarkdownAST.InlineCode(content) :: rest ->
        yield $"<code>%s{content}</code>"
        yield! renderHtmlSeq rest
    | MarkdownAST.LineBreak :: rest ->
        yield "<br>"
        yield! renderHtmlSeq rest
    | MarkdownAST.Link(url, title, content) :: rest ->
        if title = "" then
            yield $"<a href=\"%s{url}\">"
        else
            yield $"<a href=\"%s{url}\" title=\"%s{title}\">"
        yield! renderHtmlSeq content
        yield "</a>"
        yield! renderHtmlSeq rest
    | _ -> ()
}

let renderHtmlBody (ast: MarkdownAST list) =
    renderHtmlSeq ast |> String.concat ""

let renderHtml (options: CommandLineOptions) (ast: MarkdownAST list) =
    [HTML_HEADER options.title; renderHtmlBody ast; HTML_FOOTER] |> String.concat "\n"