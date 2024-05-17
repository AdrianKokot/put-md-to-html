module MdToHtml.Tests.Markdown.Blockquote

open NUnit.Framework
open MdToHtml.Modules.MarkdownParser

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ShouldParseHeaderWithEmphasis () =
    let content = "# This is some header with *emphasis*".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Header(1, [Text("This is some header with "); Emphasis([Text("emphasis")])])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseBlockQuote () =
    let content = "> This is a blockquote".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [BlockQuote([Paragraph([Text("This is a blockquote")])])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseBlockQuoteWithMultipleLines () =
    let content = "> This is a blockquote\n> with multiple lines".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [BlockQuote([Paragraph([Text("This is a blockquote"); Text("with multiple lines")])])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseBlockQuoteWithNestedBlockQuote () =
    let content = "> This is a blockquote\n>> with a nested blockquote".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [BlockQuote([Paragraph([Text("This is a blockquote")]); BlockQuote([Paragraph([Text("with a nested blockquote")])])])]
    
    Assert.That(actual, Is.EquivalentTo(expected))
