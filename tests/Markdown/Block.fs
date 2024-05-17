module MdToHtml.Tests.Markdown.Block

open NUnit.Framework
open MdToHtml.Modules.MarkdownParser

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ShouldParseHeader () =
    let content = "# This is a header".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Header(1, [Text("This is a header")])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseMultipleHeaders () =
    let content = "# Header 1\n## Header 2".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Header(1, [Text("Header 1")]); Header(2, [Text("Header 2")])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseParagraph () =
    let content = "This is a paragraph".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph([Text("This is a paragraph")])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseParagraphWithMultipleLines () =
    let content = "This is a paragraph\nwith multiple lines".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph([Text("This is a paragraph"); Text("with multiple lines")])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseParagraphWithLineBreaks () =
    let content = "This is a paragraph  \nwith line breaks".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph([Text("This is a paragraph"); LineBreak; Text("with line breaks")])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseMultipleParagraphs () =
    let content = "This is a paragraph\n\nThis is another paragraph".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph([Text("This is a paragraph")]); Paragraph([Text("This is another paragraph")])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseParagraphWithEmphasis () =
    let content = "This is a paragraph with *emphasis*".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph([Text("This is a paragraph with "); Emphasis([Text("emphasis")])])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

