module MdToHtml.Tests.Inline

open NUnit.Framework
open modules.MarkdownParser

[<SetUp>]
let Setup () = ()

[<Test>]
let ShouldParseHeaderWithEmphasis () =
    let content =
        "# This is some header with *emphasis*".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Header(1, [ Text("This is some header with "); Emphasis([ Text("emphasis") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseEmphasis () =
    let content = "*This is some text with emphasis*".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Emphasis([ Text("This is some text with emphasis") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseStrong () =
    let content = "**This is some text with strong**".Split([| '\n' |]) |> List.ofArray
    let actual = parseMarkdown content
    let expected = [ Paragraph([ Strong([ Text("This is some text with strong") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseStrongAndEmphasis () =
    let content =
        "**This is some text with _strong_ and _emphasis_**".Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Strong(
                    [ Text("This is some text with ")
                      Emphasis([ Text("strong") ])
                      Text(" and ")
                      Emphasis([ Text("emphasis") ]) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseStrongAndEmphasisInHeader () =
    let content =
        "# **This is some text with _strong_ and _emphasis_**".Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Header(
              1,
              [ Strong(
                    [ Text("This is some text with ")
                      Emphasis([ Text("strong") ])
                      Text(" and ")
                      Emphasis([ Text("emphasis") ]) ]
                ) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseTwoEmphasisInOneLine () =
    let content =
        "This is some text with *emphasis* and *emphasis2*".Split([| '\n' |])
        |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph(
              [ Text("This is some text with ")
                Emphasis([ Text("emphasis") ])
                Text(" and ")
                Emphasis([ Text("emphasis2") ]) ]
          ) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseInlineCode () =
    let content =
        "This is some text with `inline code`".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Text("This is some text with "); InlineCode("inline code") ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseInlineCodeWrappedInEmphasis () =
    let content =
        "This is some text with *`inline code`*".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Text("This is some text with "); Emphasis([ InlineCode("inline code") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))

[<Test>]
let ShouldParseInlineCodeWrappedInStrong () =
    let content =
        "This is some text with **`inline code`**".Split([| '\n' |]) |> List.ofArray

    let actual = parseMarkdown content

    let expected =
        [ Paragraph([ Text("This is some text with "); Strong([ InlineCode("inline code") ]) ]) ]

    Assert.That(actual, Is.EquivalentTo(expected))
