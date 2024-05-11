module MdToHtml.Tests.HorizontalRule

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
let ShouldParseHorizontalRuleFromThreeDashes () =
    let content = "---".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [HorizontalRule]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseHorizontalRuleFromThreeAsterisks () =
    let content = "***".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [HorizontalRule]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseHorizontalRuleFromThreeUnderscores () =
    let content = "___".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [HorizontalRule]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldNotParseHorizontalRuleFromTwoDashes () =
    let content = "--".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph([Text("--")])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldNotParseHorizontalRuleFromTwoAsterisks () =
    let content = "**".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph([Emphasis([])])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldNotParseHorizontalRuleFromTwoUnderscores () =
    let content = "__".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [Paragraph([Emphasis([])])]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseHorizontalRuleFromMoreThanThreeDashes () =
    let content = "-----".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [HorizontalRule]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseHorizontalRuleFromMoreThanThreeAsterisks () =
    let content = "*****".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [HorizontalRule]
    
    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseHorizontalRuleFromMoreThanThreeUnderscores () =
    let content = "_____".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [HorizontalRule]
    
    Assert.That(actual, Is.EquivalentTo(expected))
