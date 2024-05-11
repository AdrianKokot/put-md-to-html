module MdToHtml.Tests.CodeBlock

open NUnit.Framework
open modules.MarkdownParser

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ShouldParseCodeBlock () =
    let content = "```fsharp\nlet x = 1\n```".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [CodeBlock("fsharp", ["let x = 1"])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseCodeBlockWithMultipleLines () =
    let content = "```fsharp\nlet x = 1\nlet y = 2\n```".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [CodeBlock("fsharp", ["let x = 1"; "let y = 2"])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseCodeBlockWithEmptyLines () =
    let content = "```fsharp\nlet x = 1\n\nlet y = 2\n```".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [CodeBlock("fsharp", ["let x = 1"; ""; "let y = 2"])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseCodeBlockWithEmptyLinesAtTheEnd () =
    let content = "```fsharp\nlet x = 1\n\nlet y = 2\n\n```".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [CodeBlock("fsharp", ["let x = 1"; ""; "let y = 2"; ""])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseCodeBlockWithEmptyLinesAtTheBeginning () =
    let content = "```fsharp\n\nlet x = 1\n\nlet y = 2\n```".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [CodeBlock("fsharp", [""; "let x = 1"; ""; "let y = 2"])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseCodeBlockWithEmptyLinesAtTheBeginningAndEnd () =
    let content = "```fsharp\n\nlet x = 1\n\nlet y = 2\n\n```".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [CodeBlock("fsharp", [""; "let x = 1"; ""; "let y = 2"; ""])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseMultipleCodeBlocks () =
    let content = "```fsharp\nlet x = 1\n```\n```fsharp\nlet y = 2\n```".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [CodeBlock("fsharp", ["let x = 1"]); CodeBlock("fsharp", ["let y = 2"])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseCodeBlockWithNoLanguage () =
    let content = "```\nlet x = 1\n```".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [CodeBlock("", ["let x = 1"])]
    
    Assert.That(actual, Is.EquivalentTo(expected))
