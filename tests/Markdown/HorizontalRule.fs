module MdToHtml.Tests.Markdown.HorizontalRule

open NUnit.Framework
open MdToHtml.Modules.MarkdownParser

[<SetUp>]
let Setup () =
    ()


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
